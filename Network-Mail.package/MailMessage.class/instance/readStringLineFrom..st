parsing
readStringLineFrom: aStream 
	"Read and answer the next line from the given stream. Consume the carriage return but do not append it to the string."

	^aStream nextLine