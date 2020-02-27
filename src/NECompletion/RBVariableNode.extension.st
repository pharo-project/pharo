Extension { #name : #RBVariableNode }

{ #category : #'*NECompletion' }
RBVariableNode >> argEntriesForMethodNode: aMethodNode [
	^ aMethodNode argumentNames
		select: [ :each | each beginsWith: self name ]
		thenCollect: [ :each | NECLocalEntry contents: each node: self ]
]

{ #category : #'*NECompletion' }
RBVariableNode >> classVarEntriesForClass: lookupClass [
	^ lookupClass allClassVarNames
		select: [ :each | each beginsWith: self name ]
		thenCollect: [ :each | NECClassVarEntry contents: each node: self ]
]

{ #category : #'*NECompletion' }
RBVariableNode >> completionEntries: offset [
	| methodNode lookupClass |
	methodNode := self methodNode.
	lookupClass := methodNode compilationContext getClass.
	(self isDefinition or: [self isArgument])
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

{ #category : #'*NECompletion' }
RBVariableNode >> completionToken: offset [

	^ self name
]

{ #category : #'*NECompletion' }
RBVariableNode >> entriesSpecialVariables [
	^ #('self' 'super' 'thisContext' 'nil' 'false' 'true')
			select: [ :each | each beginsWith: self name ]
			thenCollect: [ :each | NECSpecialVariableEntry contents: each node: self ]
]

{ #category : #'*NECompletion' }
RBVariableNode >> globalEntries [
	^ Smalltalk globals keys 
			select: [ :each | each beginsWith: self name ]
			thenCollect: [ :each | NECGlobalEntry contents: each  node: self ]
]

{ #category : #'*NECompletion' }
RBVariableNode >> poolVarEntriesForClass: lookupClass [
	^ (lookupClass allSharedPools flatCollect: [ :each | each classVarNames ])
			select: [ :each | each beginsWith: self name ]
			thenCollect: [ :each | NECClassVarEntry contents: each node: self ]
]

{ #category : #'*NECompletion' }
RBVariableNode >> possibleSelectorEntries [
	^ Symbol allSymbols
		select: [ :each | (each beginsWith: self name) and: [ each contents numArgs = 0 ] ]
		thenCollect: [ :each | NECSymbolEntry contents: each node: self ]
]

{ #category : #'*NECompletion' }
RBVariableNode >> slotEntriesForClass: lookupClass [
	^lookupClass 
		allSlots select: [ :each | each name beginsWith: self name ]
		thenCollect: [ :each | NECInstVarEntry contents: each name node: self ]
]

{ #category : #'*NECompletion' }
RBVariableNode >> tempEntriesForMethodNode: aMethodNode [
	^ aMethodNode temporaryNames
		select: [ :each | each beginsWith: self name ]
		thenCollect: [ :each | NECLocalEntry contents: each node: self ]
]
