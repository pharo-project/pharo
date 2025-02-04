Extension { #name : 'OCVariableNode' }

{ #category : '*NECompletion' }
OCVariableNode >> argEntriesForMethodNode: aMethodNode [
	^ aMethodNode argumentNames
		select: [ :each | each beginsWith: self name ]
		thenCollect: [ :each | NECLocalEntry contents: each node: self ]
]

{ #category : '*NECompletion' }
OCVariableNode >> classVarEntriesForClass: lookupClass [
	^ lookupClass allClassVarNames
		select: [ :each | each beginsWith: self name ]
		thenCollect: [ :each | NECClassVarEntry contents: each node: self ]
]

{ #category : '*NECompletion' }
OCVariableNode >> completionEntries: offset [
	| methodNode lookupClass |
	methodNode := self methodNode.
	lookupClass := methodNode methodClass.
	self isDefinition
		ifTrue: [ ^ self possibleSelectorEntries ].

	^ Array streamContents: [ :str |
		str
			nextPutAll: (self globalEntries);
			nextPutAll: (self slotEntriesForClass: lookupClass);
			nextPutAll: (self tempEntriesForMethodNode: methodNode);
			nextPutAll: (self argEntriesForMethodNode: methodNode);
			nextPutAll: (self entriesSpecialVariables);
			nextPutAll: (self classVarEntriesForClass: lookupClass);
			nextPutAll: (self poolVarEntriesForClass: lookupClass) ]
]

{ #category : '*NECompletion' }
OCVariableNode >> completionToken: offset [

	^ self name
]

{ #category : '*NECompletion' }
OCVariableNode >> entriesSpecialVariables [
	^ #('self' 'super' 'thisContext' 'nil' 'false' 'true')
			select: [ :each | each beginsWith: self name ]
			thenCollect: [ :each | NECSpecialVariableEntry contents: each node: self ]
]

{ #category : '*NECompletion' }
OCVariableNode >> globalEntries [
	^ Smalltalk globals keys
			select: [ :each | each beginsWith: self name ]
			thenCollect: [ :each | NECGlobalEntry contents: each  node: self ]
]

{ #category : '*NECompletion' }
OCVariableNode >> poolVarEntriesForClass: lookupClass [
	^ (lookupClass allSharedPools flatCollect: [ :each | each classVarNames ])
			select: [ :each | each beginsWith: self name ]
			thenCollect: [ :each | NECClassVarEntry contents: each node: self ]
]

{ #category : '*NECompletion' }
OCVariableNode >> possibleSelectorEntries [
	^ Symbol allSymbols
		select: [ :each | (each beginsWith: self name) and: [ each contents numArgs = 0 ] ]
		thenCollect: [ :each | NECSymbolEntry contents: each node: self ]
]

{ #category : '*NECompletion' }
OCVariableNode >> slotEntriesForClass: lookupClass [
	^lookupClass
		allSlots select: [ :each | each name beginsWith: self name ]
		thenCollect: [ :each | NECInstVarEntry contents: each name node: self ]
]

{ #category : '*NECompletion' }
OCVariableNode >> tempEntriesForMethodNode: aMethodNode [
	^ aMethodNode temporaryNames
		select: [ :each | each beginsWith: self name ]
		thenCollect: [ :each | NECLocalEntry contents: each node: self ]
]
