tests - bits
testPngSuite
	"Requires the suite from 
		ftp://swrinde.nde.swri.edu/pub/png/images/suite/PngSuite.zip
	to be present as PngSuite.zip"
	| file |
	file := [ FileStream readOnlyFileNamed: 'PngSuite.zip'] on: Error do:[:ex| ex return].
	file ifNil:[^self].
	[ | zip entries |zip := ZipArchive new readFrom: file.
	entries := zip members select:[:mbr| mbr fileName asLowercase endsWith: '.png'].
	entries do:[:mbr| 
		(mbr fileName asLowercase first = $x)
			ifTrue: [self encodeAndDecodeWithError: mbr contentStream ]
			ifFalse: [self encodeAndDecodeStream: mbr contentStream ] ].
	] ensure:[file close].