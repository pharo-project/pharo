generating C program
writeCIncludesOn: stream
	"Internal method for the C program creation. It writes C #include definitions for all the header names. It includes the mandatry ones for our C program (such as stdio.h and stdlib.h) and then all the user-defined ones which should define the constants defined as class variables in the shared pool."
	self writeCIncludes: #('stdio.h' 'stdlib.h') on: stream.
	self writeCIncludes: self headersToInclude on: stream.
	stream cr.
	
	
