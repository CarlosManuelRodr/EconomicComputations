/* Program ADGeo, written by Hector Olivares.
   olivares@th.physik.uni-frankfurt.de

   It performs the discrete Anderson-Darling test for a Geometric
   probability distribution using parametric bootstrap, as it is
   described in Bracquemond, Cretois and Gaudoin, 2002.

   The geometric distribution is considered to be in the form:

                            k
           P(X = k) = p(1-p)

   with k = 0,1,2,... and p the probability of success of the rela-
   ted Poisson process. 

   The data is read from a file containing the counts, in the follo-
   wing format:

   Counts for 1,
   Counts for 2,
   ...
   Counts for n.

   The p value of the tested distribution is displayed on the screen.

*/

#include "stdio.h"
#include "math.h"  // For pow and floor functions
#include "ran2.h"  // For pseudo-random generator from Numerical Recepies

#define nMax 500 // Lenght of a vector containing the data read from the file.
#define nBootstrap 1000 // Number of replics for the parametric bootstrap
// Function declarations
// Main
int main(
    int argc,
    char *argv[]
    );


// Function for finding the Anderson-Darling statistic
double AD(
    int n,  // Number of events
    int b,  // Maximum number of bins
    double E[nMax],  // Empirical distribution function
    double T[nMax]   // Theoretical (cumulative) distribution function
    );

// Funtion to build a replic of the observed data, to be used for the parametric bootstrap
// Returns the number of the last bin with empirical data.
int replic(
    int n,  // Number of events
    double p,  // Estimated p
    int nTested[],  // Vector of "counts"
    double fEDF[],   // "Empirical" cumulative distribution function
    long int *idum   // Pointer to the seed of the random generator
    );


// Function prototypes
// Function for finding the Anderson-Darling statistic
double AD(int n,int b,double E[nMax],double T[nMax])
    {
    double fAD;
    fAD = ((E[0] - T[0])*(E[0] - T[0])*T[0])/(T[0]*(1.0f - T[0]));
    int i=1; //counter
//    printf("%0.9f\n",fAD);
    // Henze (1996) truncation condition
    while ((i<b)&&!((E[i+1] - E[i] < 1e-12)&&(double)n*pow((1.0f-T[i]),3.0f) < 1e-3))
    {
            fAD += ((E[i] - T[i])*(E[i] - T[i])*(T[i] - T[i - 1]))/(T[i]*(1.0f - T[i]));
//            printf("%0.9f\n",fAD);
    i++;
    }
    return((double)n*fAD);
}

// Funtion to build a replic of the observed data, to be used for the parametric bootstrap
int replic(
    int n,
    double p,
    int nTested[],
    double fEDF[],
    long int *idum
    )
{

    int i, j; // Counters
    int nProvMax = 0; // Provisional maximum number of bins.

    int nTotalBins = 0; // Number of total bins, to return.
    int k; // The random variable of the geometric distribution.

    // Fill the vector of counts
    nTested[0] = 0;

    for(i = 0; i < n; i++){
        // Generate the random variable
        k = (int)floor(log(1 - ran2(idum))/log(1-p));
        if (k > nProvMax){
            // If the random variable is greater than its last maximum value,
            // fill the intermediate bins with zeros.
            for(j = nProvMax + 1; j <= k; j++) nTested[j] = 0;
            nProvMax = k;
//            for(j = 0; j < nProvMax+1; j++) printf("%d\t",nTested[j]);
//            printf("\n");
        }
        // Increment by one the count in the respective bin
        nTested[k]++;
    }

    nTotalBins = nProvMax + 1;

    // Fill with zeroes the EDF vector
    for(i = 0; i < nTotalBins + 10; i++) fEDF[i] = 0;
    // Compute the EDF
    fEDF[0] = (double)nTested[0];
    for(i = 1; i < nTotalBins; i++) fEDF[i] = fEDF[i-1] + (double)nTested[i];
    // Normalize    
    for(i = 0; i < nTotalBins; i++) fEDF[i] /= n;
    // In case Henze's condition is not satisfied whith number of bins, add ten more
    for(i = nTotalBins; i < nTotalBins+10; i++) fEDF[i] = 1.0f;

    return(nTotalBins);
}

// Main function
int main(
    int argc,
    char *argv[]
    )
{

FILE *DataFile = NULL; // Pointer to file containing the data (counts)
//char szID[30]; // String that will contain the first line of the file
int nTested[nMax] = {0}; // Vector containing first the data read from the file, and then each replic for the bootstrap
double fEDF[nMax] = {0}; // Vector for storing the empirical cumulative distribution functions
double fTDF[nMax] = {0}; // Vector containing the values of the theoretical cumulative distribution function
double fAD, fADTemp; // Anderson-Darling statistic of the empirical data and of the replics for the bootstrap
int nAlpha; // Count of AD statistics greater than the one of the data
double fPValue; // p-value

// Interpret the 2nd parameter as the name of the file
if (argc >= 2){

    DataFile = fopen(argv[1], "r"); // Open the data file in reading mode

    if (DataFile == NULL)
    {
        printf("ERROR: File '%s' could not be opened. Are you sure it exists?\n", argv[1]);
        return(0);
    }
}
else {
    printf("ERROR: Please supply the name of the file to read.\n");
    return(0);
}

// Output name of the file

printf("%s\n",argv[1]);

// Loop to read the file.
// At the same time, compute the total number of events, the sum of the values of all events
// And the entries of the EDF whithout normalization

int i = 0;
int nEvents = 0;
int nSumOfValues = 0;
while(fscanf(DataFile, "%d", &nTested[i]) == 1)
{
//    printf("%d\t%d\n", i, nTested[i]);
    nEvents += nTested[i];
    fEDF[i] = (double)nEvents;
    nSumOfValues += (i+1)*nTested[i];
    i++;
}

int nTotalBins = i; // Maximum number of bins on which the statistic is to be computed 


// Close the data file
fclose(DataFile);

////////////////////////////////////////////////////////////////////

// Compute Maximum Likelihood estimator for p
double p = (double)nEvents/(double)nSumOfValues;
printf("n : %d    Sum(Ki) : %d\n", nEvents, nSumOfValues);
printf("Maximun Likelihood estimator for p: %0.9f\n", p);

// Compute TDF
// (I add ten just in case that the number of bins computed before in the empirical data doesn't satisfy Henze's (1996) truncation criterion)
for (i=0;i<nTotalBins+10;i++) {
    if (i == 0) fTDF[i] = p;
    else fTDF[i] = fTDF[i-1] + p*pow((1.-p),(double)i);
}

// Normalize the EDF
// (I add ten just in case that the number of bins computed before in the empirical data doesn't satisfy Henze's (1996) truncation criterion)
for (i=0;i<nTotalBins+10;i++) {
    if (i <nTotalBins) fEDF[i] /= (double)nEvents;
    else fEDF[i] = 1.0f;
//    printf("%d\t%f\t%f\n", i, fEDF[i],fTDF[i]);
}


// Find the Anderson-Darling statistic for the empirical data

fAD = AD(nEvents,nTotalBins+10,fEDF,fTDF);

printf("AD = %0.9f\n",fAD);

/////////////////////////////////////////////
// Perform bootstrap.
// Provide seed from pseudo-random generator

long int idum = -1;

int j;

nAlpha = 0;

for (i = 0; i < nBootstrap; i++){
// Create a replic of the theoretical process
// The replic overwrites the data already present in the vectors nTested and fEDF
//    printf("Replic %d\t",i);
    nTotalBins = replic(nEvents,p,nTested,fEDF,&idum);
//    for(j = 0; j < nTotalBins+10; j++) printf("%d\t%d\t%f\n",j,nTested[j],fEDF[j]);
//    printf("\n");

// Obtain the AD statistic of the replic
    fADTemp = AD(nEvents,nTotalBins+10,fEDF,fTDF);

// Check if it is greater than that of the empirical data
    if (fADTemp >= fAD) nAlpha ++;
}

fPValue = (double)nAlpha/(double)nBootstrap;

printf("Greater: %d/%d    p-value: %f\n",nAlpha,nBootstrap,fPValue);

return(0);

}

