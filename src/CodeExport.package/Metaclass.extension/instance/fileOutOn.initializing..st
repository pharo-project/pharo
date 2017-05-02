*CodeExport
fileOutOn: aFileStream initializing: aBool

	<localMetaclassMethod> 
    "This method is supposed to be local in Metaclass because of a good reason.
    We use this pragma to test if Metaclass does not contain some accidental 
    local selectors."

	super fileOutOn: aFileStream.
	(aBool and:[ self includesSelector: #initialize ]) ifTrue: [
		aFileStream cr.
		aFileStream cr.
		aFileStream nextChunkPut: thisClass name , ' initialize'.
		aFileStream cr ]