filein/out
fileIn: fullFileName
	"FileIn the contents of a gzipped file"
	| zipped unzipped |
	zipped := self on: (FileStream readOnlyFileNamed: fullFileName).
	unzipped := MultiByteBinaryOrTextStream with: (zipped contents asString).
	unzipped reset.
	unzipped fileIn.
