#include <stdlib.h>
#include <string.h>

char* concat(const char *s1, const char *s2)
{
    char *result = malloc(strlen(s1)+strlen(s2)+1); // +1 for the null-terminator
    if(result != NULL) {
    	strcpy(result, s1);
    	strcat(result, s2);
    }
    
    return result;
}