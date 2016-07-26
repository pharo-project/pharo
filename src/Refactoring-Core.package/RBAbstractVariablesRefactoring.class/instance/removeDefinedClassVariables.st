transforming
removeDefinedClassVariables
	| selectionBlock nonMetaClass |
	nonMetaClass := fromClass theNonMetaClass.
	selectionBlock := 
	[ :varName | 
	(toClasses 
		detect: 
			[ :each | 
			(each theNonMetaClass includesClass: (nonMetaClass whoDefinesClassVariable: varName)) not ]
		ifNone: [ nil ]) notNil ].
	classVarReaders := classVarReaders select: selectionBlock.
	classVarWriters := classVarWriters select: selectionBlock