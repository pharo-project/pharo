DEPRECATED
initializeVariablesFromStonExtracted
	| stonStream parsedDict |
	stonStream := (Smalltalk os cHeaderExtractorFor: self) outputFilename asFileReference readStream. 
	parsedDict := STON fromStream: stonStream.
	"The STON file looks like this:
	{
        'SIGHUP' : [
                1,
                4
        ],
        'SIGKILL' : [
                9,
                4
        ],
        'SIGTERM' : [
                15,
                4
        ]
}
	"
	parsedDict keysAndValuesDo: [ :key :value |
		self classVarNamed: key put: value first.	
 	]