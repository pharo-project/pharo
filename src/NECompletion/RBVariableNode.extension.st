Extension { #name : #RBVariableNode }

{ #category : #'*NECompletion' }
RBVariableNode >> completionEntries [
	| methodNode lookupClass |
	methodNode := self methodNode.
	lookupClass := methodNode compilationContext getClass.
	self isDefinition ifTrue: [ ^ (self select: Symbol allSymbols beginningWith: self name) select: [ :each | each numArgs = 0 ] ].
   self isArgument ifTrue: [ ^ (self select: Symbol allSymbols beginningWith: self name) select: [ :each | each numArgs = 0 ] ].
	"using a stream to store results should be better"
	^	(self select: Smalltalk globals keys beginningWith: self name), 
	 	(self select: (lookupClass allSlots collect: [ :each | each name ]) beginningWith: self name),
		(self select: methodNode temporaryNames beginningWith: self name),
		(self select: methodNode argumentNames beginningWith: self name),
		(self select: lookupClass allClassVarNames asOrderedCollection beginningWith: self name),
		(self select: (lookupClass allSharedPools flatCollect: [ :each | each classVarNames]) beginningWith: self name)

]

{ #category : #'*NECompletion' }
RBVariableNode >> completionToken [

	^ self name
]
