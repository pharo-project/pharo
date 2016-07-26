filein/out
fileOut
	"FileOut and then change the properties of the file so that it won't be
	treated as text by, eg, email attachment facilities"

	super fileOut.
	(FileStream oldFileNamed: self name , '.st') close