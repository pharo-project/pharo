Extension { #name : #RBProgramNode }

{ #category : #'*RewriteTool' }
RBProgramNode >> command [
	^ self propertyAt: #command
]

{ #category : #'*RewriteTool' }
RBProgramNode >> command: anObject [
	self propertyAt: #command put: anObject
]

{ #category : #'*RewriteTool' }
RBProgramNode >> conditionalSelectors [
	| selectors |
	selectors := OrderedCollection new.
	selectors
		add: #ifFalse:;
		add: #ifFalse:ifTrue:;
		add: #ifNil:;
		add: #ifNil:ifNotNil:;
		add: #ifNotNil:;
		add: #ifNotNil:ifNil:;
		add: #ifTrue:;
		add: #ifTrue:ifFalse:;
		add: #or:;
		add: #and:.
	^ selectors
]

{ #category : #'*RewriteTool' }
RBProgramNode >> getSequenceParent [
	^ self isSequence
		ifTrue: [ self ]
		ifFalse: [ self parent getSequenceParent ]
]

{ #category : #'*RewriteTool' }
RBProgramNode >> initialize [
	self oldNodes: OrderedCollection new
]

{ #category : #'*RewriteTool' }
RBProgramNode >> isInTemporariesList [
	^ false
]

{ #category : #'*RewriteTool' }
RBProgramNode >> oldNodes [
	^ self propertyAt: #oldNodes
]

{ #category : #'*RewriteTool' }
RBProgramNode >> oldNodes: anObject [
	self propertyAt: #oldNodes put: anObject
]

{ #category : #'*RewriteTool' }
RBProgramNode >> optionsForNode [
	| options |
	options := OrderedCollection new.
	self isInTemporariesList
		ifFalse: [ options add: #('Change/Rename' #change) ].
	self oldNodes ifNotEmpty: [ options add: #('Undo on node' #undoOnNode) ].
	^ options
]
