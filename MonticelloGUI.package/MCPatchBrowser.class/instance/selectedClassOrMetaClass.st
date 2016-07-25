subclassresponsibility
selectedClassOrMetaClass
	| definition |
	selection ifNil: [ ^nil ].
	(definition := selection definition) ifNil: [ ^nil ].
	definition isMethodDefinition ifFalse: [ ^nil ].
	^definition actualClass