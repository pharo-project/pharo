class tags
removeClassDefinitionName: aClassName fromClassTag: aSymbol
	"Detags the class aClass with the tag aSymbol"
	| tag |
	
	tag := self classTagNamed: (self toTagName: aSymbol) ifAbsent: [ ^ self ].
	tag removeClassNamed: aClassName.
	tag isEmpty 
		ifTrue: [ self basicRemoveTag: tag ].
