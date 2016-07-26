private
toTagName: aSymbol
	^ (aSymbol beginsWith: self name, '-')
		ifTrue: [ (aSymbol allButFirst: self name size + 1) asSymbol ]
		ifFalse: [ aSymbol ]