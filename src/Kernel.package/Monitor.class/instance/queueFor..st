private
queueFor: aSymbol 
	aSymbol ifNil: [^self defaultQueue].
	^self queueDict at: aSymbol ifAbsentPut: [OrderedCollection new]