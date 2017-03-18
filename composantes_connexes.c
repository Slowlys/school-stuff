#include "limace.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#define TAB_SIZE 1024

#define ERR_PARAMETRES          		1
#define ERR_RECUPERATION_IMAGE  	2

/* ------------------------------------------------------------------- 
 * Fonctions utiles
 -------------------------------------------------------------------  */
typedef enum {
	V4=0,
	V8=1
}TypeVoisinage;

int max(int x,int y){
	int res=x>y?x:y;
	return res;
}
int min(int x,int y){
	int res=x<y?x:y;
	return res;
}


void saveResult(Matrix res,const char* fileName){
	int** data=MatGetInt(res);
	int x,y;
	int labelMax=0;
	int newLabel;
	int *newLabels;
	
	/*find max value of labels */
	for(y=0;y<MatNbRow(res);y++){
		for(x=0;x<MatNbCol(res);x++){
			labelMax=max(data[y][x],labelMax);
		}
	}
	labelMax++;
	newLabels=(int*)malloc(sizeof(int)*labelMax);
	for(x=0;x<labelMax;x++){
		newLabels[x]=-1;
	}
	/*create continious labels*/
	newLabels[0]=0;
	newLabel=1;
	
	for(y=0;y<MatNbRow(res);y++){
		for(x=0;x<MatNbCol(res);x++){
			if(newLabels[data[y][x]]==-1){
				newLabels[data[y][x]]=newLabel;
				newLabel++;
			}
		}
	}
	for(y=0;y<MatNbRow(res);y++){
		for(x=0;x<MatNbCol(res);x++){
			data[y][x]=newLabels[data[y][x]];
		}
	}
	
	/*save result as image*/
	{
		Image image=ImCAlloc(Color,MatNbRow(res),MatNbCol(res));
		unsigned char** red=ImGetR(image);
		unsigned char** green=ImGetG(image);
		unsigned char** blue=ImGetB(image);
		long step=(256*256*256)/newLabel;
		for(y=0;y<MatNbRow(res);y++){
			for(x=0;x<MatNbCol(res);x++){
				int etiquette=data[y][x];
				long idxColor=step*etiquette;
				blue[y][x]=idxColor/(256*256);
				idxColor=idxColor%(256*256);
				green[y][x]=idxColor/256;
				red[y][x]=idxColor%256;
			}
		}
		ImWrite(image,fileName);
		
		ImFree(&image);
	}
	
	free(newLabels);
	
}

/*create array to save equivalence*/
int* creerTableau(){
	int i;
	int* tab=(int*)malloc(sizeof(int)*TAB_SIZE);
	
	for( i=0;i<TAB_SIZE;i++){
		tab[i] = -1;
	}	
	return tab;
}


/* add label and increment next label value*/
void ajoutEtiquette(int* equivalences, int* nbEtiquettes){
	equivalences[*nbEtiquettes]=*nbEtiquettes;
	*nbEtiquettes=(*nbEtiquettes)+1;
}



/* ------------------------------------------------------------------- 
 * To do
 -------------------------------------------------------------------  */
 
Matrix coordonneesVoisins(Image image,int u,int v, TypeVoisinage voisinage){
	int nbLignes = ImNbRow(image);
	int nbColonnes = ImNbCol(image);	
	Matrix coords=NULL;
	Matrix resultat=NULL;
	int indice_voisin = 0;
	int **acces_matrice, **acces_resultat;
	int i;
	
	coords = MatAlloc(Int, 2, 8);
	acces_matrice = MatGetInt(coords);
		
	if (u - 1 >= 0) {
		acces_matrice[0][indice_voisin] = u - 1;
		acces_matrice[1][indice_voisin] = v;
		indice_voisin++;
	}
	
	if ( u + 1 < nbLignes) {
		acces_matrice[0][indice_voisin] = u + 1;
		acces_matrice[1][indice_voisin] = v;
		indice_voisin++;
	}
	
	if ( v - 1 >= 0) {
		acces_matrice[0][indice_voisin] = u;
		acces_matrice[1][indice_voisin] = v - 1;
		indice_voisin++;
	}
	
	if ( v + 1 < nbColonnes) {
		acces_matrice[0][indice_voisin] = u;
		acces_matrice[1][indice_voisin] = v + 1;
		indice_voisin++;
	}
	
	if (voisinage == V8 ) {
		if ( u - 1 >= 0 ) {			
			if ( v - 1 >= 0 ) {
				acces_matrice[0][indice_voisin] = u - 1;
				acces_matrice[1][indice_voisin] = v - 1;
				indice_voisin++;
			}
			
			if ( v + 1 < nbColonnes ) {
				acces_matrice[0][indice_voisin] = u - 1;
				acces_matrice[1][indice_voisin] = v + 1;
				indice_voisin++;
			}
		}
		
		if ( u + 1 < nbLignes ) {
			if (v - 1 >= 0) {
				acces_matrice[0][indice_voisin] = u + 1;
				acces_matrice[1][indice_voisin] = v - 1;
				indice_voisin++;
			}
			
			if (v + 1 < nbColonnes) {
				acces_matrice[0][indice_voisin] = u + 1;
				acces_matrice[1][indice_voisin] = v + 1;
				indice_voisin++;
			}
		}
	}
	
	resultat = MatAlloc(Int, 2, indice_voisin);
	acces_resultat = MatGetInt(resultat);
	
	for (i = 0; i < indice_voisin; ++i) {
		acces_resultat[0][i] = acces_matrice[0][i];
		acces_resultat[1][i] = acces_matrice[1][i];
	}

	MatFree(&coords);
	return resultat;
}


int findCC(int* equivalences, int nbEtiquettes, int etiquette) {
	assert((etiquette >= 0) && (etiquette < nbEtiquettes));
	if (equivalences[etiquette] == etiquette) {
		return etiquette;
	} else {
		return findCC(equivalences, nbEtiquettes, equivalences[etiquette]);
	}
}


void unionCC(int* equivalences, int nbEtiquettes, int etiquette1, int etiquette2){
	int ref1, ref2;
		
	ref1 = findCC(equivalences, nbEtiquettes, etiquette1);
	ref2 = findCC(equivalences, nbEtiquettes, etiquette2);
		
	equivalences[ref2] = ref1;
}

void stocker_equivalences (int suiteVoisins, int premiereEtiquetteTrouvee, int **acces_voisins, int nbVoisins,
                           int nbEtiquettes, int *equivalences, int **acces_etiquettesPixels) {
	int ligneVoisin, colonneVoisin;

	for (	; suiteVoisins < nbVoisins; ++suiteVoisins) {
		ligneVoisin = acces_voisins[0][suiteVoisins];
		colonneVoisin = acces_voisins[1][suiteVoisins];			
		if ((acces_etiquettesPixels[ligneVoisin][colonneVoisin] > 0) && 
		    (acces_etiquettesPixels[ligneVoisin][colonneVoisin] != premiereEtiquetteTrouvee)) {
			unionCC(equivalences, nbEtiquettes, premiereEtiquetteTrouvee, acces_etiquettesPixels[ligneVoisin][colonneVoisin]);
		}
	}
}

void donner_etiquette (int lignePixel, int colonnePixel, Matrix voisins, int **acces_etiquettesPixels, int *equivalences, int *nbEtiquettes) {
	int **acces_voisins;
	int i, ligneVoisin, colonneVoisin, nbVoisins, premiereEtiquetteTrouvee;

	nbVoisins = MatNbCol(voisins);	
	acces_voisins = MatGetInt(voisins);
		
	i = 0;
	do {
		ligneVoisin = acces_voisins[0][i];
		colonneVoisin = acces_voisins[1][i];
		premiereEtiquetteTrouvee = acces_etiquettesPixels[ligneVoisin][colonneVoisin];
		++i;
	} while ((premiereEtiquetteTrouvee <= 0) && (i < nbVoisins));

	if (premiereEtiquetteTrouvee <= 0)  {
		acces_etiquettesPixels[lignePixel][colonnePixel] = *nbEtiquettes;
		ajoutEtiquette(equivalences, nbEtiquettes);	
	} else {
		acces_etiquettesPixels[lignePixel][colonnePixel] = premiereEtiquetteTrouvee;
		stocker_equivalences (i, premiereEtiquetteTrouvee, acces_voisins, nbVoisins, *nbEtiquettes, equivalences, acces_etiquettesPixels);
	}
}

Matrix composantes_connexes (Image image, TypeVoisinage voisinage) {
	int *equivalences;
	int **acces_etiquettesPixels;
	unsigned char **matrice_image;
	int nbEtiquettes, i, j;
	Matrix voisins, etiquettesPixels;
	
	int nbLignes = ImNbRow(image);
	int nbColonnes = ImNbCol(image);

	etiquettesPixels = MatCAlloc(Int, nbLignes, nbColonnes);
	acces_etiquettesPixels = MatGetInt(etiquettesPixels);

	matrice_image = ImGetI(image);

	equivalences = creerTableau();
	nbEtiquettes = 0;
	
	ajoutEtiquette(equivalences, &nbEtiquettes);
	
	for (i = 0; i < nbLignes; ++i) {
		for (j = 0; j < nbColonnes; ++j) {
			if (matrice_image[i][j] == 1) {
				voisins = coordonneesVoisins(image, i, j, voisinage);
				donner_etiquette (i, j, voisins, acces_etiquettesPixels, equivalences, &nbEtiquettes);
			}
		}
	}

	for (i = 0; i < nbLignes; ++i) {
		for (j = 0; j < nbColonnes; ++j) {
			if (matrice_image[i][j] == 1) {
				acces_etiquettesPixels[i][j] = findCC(equivalences, nbEtiquettes, acces_etiquettesPixels[i][j]);				
			}
		}
	}

	free(equivalences);
	return etiquettesPixels;
}

/*-------------------------------------------------------------------------*/

void afficher_usage(char *nom_programme) {
	fprintf(stderr, "Usage : %s <image> <composantes_connexes.ppm> <type de voisinage (4 ou 8) >\n", nom_programme);
}


int main(int argc, char **argv){
	Image image;
	Matrix resultat;
	int i, nbEtiquettes, typeVoisinage;
	int *equivalences;
	
	nbEtiquettes = 13;
	
	/* Récupération des paramètres */
	if (argc != 4) {
		fprintf(stderr, "Il faut 3 paramètres\n");
		afficher_usage(argv[0]);
		exit(ERR_PARAMETRES);
	}
	
	image = ImRead(argv[1]);
	if (!image) {
		fprintf(stderr, "L'image %s n'a pas pu être récupérée", argv[1]);
		afficher_usage(argv[0]);
		exit(ERR_RECUPERATION_IMAGE);
	}
	
	resultat = coordonneesVoisins(image, 10, 10, V4);
	printf("10, 10, V4 :\n");
	MatWriteAsc(resultat, "");
	MatFree(&resultat);
				
	resultat = coordonneesVoisins(image, 10, 10, V8);
	printf("10, 10, V8 :\n");
	MatWriteAsc(resultat, "");
	MatFree(&resultat);
	
	resultat = coordonneesVoisins(image, 0, 0, V4);
	printf("0 0 V4 ;\n");
	MatWriteAsc(resultat, "");
	MatFree(&resultat);
	
	resultat = coordonneesVoisins(image, 0, 0, V8);
	printf("0 0 V8\n");
	MatWriteAsc(resultat, "");
	MatFree(&resultat);
	
	resultat = coordonneesVoisins(image, 399, 599, V4);
	printf(" 399 599 V4\n");
	MatWriteAsc(resultat, "");
	MatFree(&resultat);
	
	resultat = coordonneesVoisins(image, 399, 599, V8);
	printf("399 599 V8\n");
	MatWriteAsc(resultat, "");
	MatFree(&resultat);
				
	/* Exercice 2 */
	equivalences = creerTableau();
	
	for (i = 0; i < nbEtiquettes; ++i) {
		equivalences[i] = i;
	}
	
	unionCC(equivalences, nbEtiquettes, 1, 2);
	unionCC(equivalences, nbEtiquettes, 4, 1);
	unionCC(equivalences, nbEtiquettes, 10, 11);
	unionCC(equivalences, nbEtiquettes, 10, 4);
	unionCC(equivalences, nbEtiquettes, 7, 8);
	unionCC(equivalences, nbEtiquettes, 6, 7);
	
	printf("Résultats Exercice 2 \n\n");
	
	for (i = 0; i < nbEtiquettes; ++i) {
		printf("%d\n", findCC(equivalences, nbEtiquettes, i));
	}
	free(equivalences);
	printf("Exercice 3\n");

	typeVoisinage = atoi(argv[3]);

	if (typeVoisinage == 4) {
		resultat = composantes_connexes (image, V4);
		saveResult(resultat, argv[2]);		
	} else if (typeVoisinage == 8) {
		resultat = composantes_connexes (image, V8);
		saveResult(resultat, argv[2]);			
	} else {
		fprintf(stderr, "Le voisinage %d n'est pas correcte\n", typeVoisinage);
		afficher_usage(argv[0]);
		exit(ERR_PARAMETRES);
	}

	ImFree(&image);
	MatFree(&resultat);
	exit(0);
}
