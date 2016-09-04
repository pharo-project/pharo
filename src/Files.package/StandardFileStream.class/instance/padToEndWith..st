accessing
padToEndWith: aChar
	"On the Mac, files do not truncate.  One can delete the old file and write a new one, but sometime deletion fails (file still open? file stale?).  This is a sad compromise.  Just let the file be the same length but pad it with a harmless character."

	| pad |
	self atEnd ifTrue: [^ self].
	pad := self isBinary 
		ifTrue: [aChar asCharacter asciiValue]	"ok for char or number"
		ifFalse: [aChar asCharacter].
	self nextPutAll: (self collectionSpecies new: ((self size - self position) min: 20000) 
							withAll: pad).