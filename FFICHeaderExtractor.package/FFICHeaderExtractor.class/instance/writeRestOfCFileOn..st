generating C program
writeRestOfCFileOn: stream
	"Internal method for the C program creation. This one writes the end of the file, including the main(), the definition of our functions, and the opening of the FILE stream for the output. printDefinedConstant() and printUndefinedConstant() are called for each constant."
	stream cr.
	stream nextPutAll: '#define printDefinedConstant(file, const) fprintf(file, "#(%s %d %ld)\n", #const, const, sizeof(const))
	#define printUndefinedConstant(file, const) fprintf(file, "#(UNDEFINED%s %d %d)\n", #const, 0, 0)

	int main(int argc, const char * argv[]) {

	    FILE *file = fopen("', self outputFilename, '", "w");
	    if (file == NULL)
	    {
	        printf("Error opening file!\n");
	        exit(1);
	    }
	'.
	self writeListOfConstantsOn: stream. 

	stream nextPutAll: '
	    fclose(file);    
	    return 0;
	}
	'
