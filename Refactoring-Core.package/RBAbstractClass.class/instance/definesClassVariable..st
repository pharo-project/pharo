testing
definesClassVariable: aSymbol 
	self realClass isTrait ifTrue: [^false].
	(self directlyDefinesClassVariable: aSymbol) ifTrue: [^true].
	^self superclass notNil 
		and: [self superclass definesClassVariable: aSymbol]