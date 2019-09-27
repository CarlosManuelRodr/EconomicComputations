/* This program generates fake data with a geometric
   distribution in order to test the program ADGeo */

#include "stdio.h"
#include "math.h"
#include "ran2.h"



void TestFunction(long int *x){

int i;
double p = 0.5;
for (i = 0; i<10; i++) printf("%f\n",floor(log(1 - ran2(x))/log(1-p)));
printf("\n");
}

int main(){

int i;

long int idum = -1;

for (i = 0; i<3; i++) TestFunction(&idum);



return(0);
}
