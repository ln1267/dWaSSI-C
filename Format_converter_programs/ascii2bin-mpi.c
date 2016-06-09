#include <stdio.h>
#include <stdlib.h>

long long main(int argc, char* argv[]){
	long int i,j;
	float **a;
	char filename[256],bin_file[256];
	FILE *fr,*fw;
	long int m,n;
	char *ptrm,*ptrn;
	if (argc > 1){
		for ( i=0; i<argc; i++){
			if (argv[i][0] == '-'){
				if (argv[i][1] == 'i'){
					sprintf(filename,"%s",argv[i+1]);
				}else if (argv[i][1] == 'o'){
					sprintf(bin_file,"%s",argv[i+1]);
				}else if (argv[i][1] == 'm'){
					m = strtol(argv[i+1],&ptrm,10);
				}else if (argv[i][1] == 'n'){
					n = strtol(argv[i+1],&ptrn,10);
				}
			}
		}
		fprintf (stdout,"Processing %s to convert in binary file %s.\n",filename,bin_file);
		fprintf (stdout,"m = %ld, n =%ld\n",m,n);
	}else{
		fprintf (stderr,"Invalid arguments:\n");
		fprintf (stderr,"Options:\n"
				"-i  Filename to input ASCII file\n"
				"-o  Filename of output Binary file\n"
				"-m  Number of lines in the file\n"
				"-n  Number of columns in each line\n");
		abort();
	}

	a = (float**) malloc (m * sizeof(float*));
	if (a == NULL){
		fprintf(stderr,"error allocating memory block for read operation.\nAborting...");
	}
	for (i=0; i<m; i++){
		a[i] = (float*) malloc (n * sizeof(float));
		if (a[i] == NULL){
			fprintf(stderr,"error allocating a[%d] row\nAborting...",i);
			abort();
		}
	}



	fr=fopen(filename,"r");
	i=0;
	while(!feof(fr)){
		for (j=0;j<n;j++)
			fscanf(fr,"%f%*c",&a[i][j]);
		i++;
	}
	fclose(fr);

	fw=fopen(bin_file,"wb");

	for (i=0; i<m; i++){
		for (j=0; j<n; j++){
			fwrite(&a[i][j],sizeof(float),1,fw);
		}
	}
	fclose(fw);


}
