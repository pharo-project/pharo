generating C program
writeCIncludes: aListOfHeaderNames on: stream
	"Internal method for the C program creation. It writes each of the user-defined header names (aListOfHeaderNames) as a C #include definition."
	aListOfHeaderNames do: [ :each | self writeCInclude: each on: stream ]
