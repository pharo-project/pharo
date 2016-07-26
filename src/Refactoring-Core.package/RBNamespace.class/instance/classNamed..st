accessing-classes
classNamed: aSymbol 
	| class classes index |
	aSymbol isNil ifTrue: [ ^ nil ].
	(self hasRemoved: aSymbol) ifTrue: [ ^ nil ].
	(newClasses includesKey: aSymbol) ifTrue: [ ^ (newClasses at: aSymbol) first ].
	(changedClasses includesKey: aSymbol) ifTrue: [ ^ (changedClasses at: aSymbol) first ].
	class := environment 
		at: aSymbol
		ifAbsent: [ nil ].
	(class isBehavior or: [ class isTrait ]) ifTrue: 
		[ classes := self createNewClassFor: class.
		^ class isMeta 
			ifTrue: [ classes last ]
			ifFalse: [ classes first ] ].
	index := aSymbol 
		indexOfSubCollection: ' class'
		startingAt: 1
		ifAbsent: [ ^ nil ].
	class := self classNamed: (aSymbol 
			copyFrom: 1
			to: index - 1) asSymbol.
	^ class isNil 
		ifTrue: [ nil ]
		ifFalse: [ class theMetaClass ]