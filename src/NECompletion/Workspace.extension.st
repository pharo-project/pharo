Extension { #name : #Workspace }

{ #category : #'*necompletion' }
Workspace >> completionAdditionals [
	| additionals |
	additionals := SortedCollection new.
	bindings ifNotNil: 
		[ bindings keysDo: 
			[ :each | 
			additionals add: (NECLocalEntry 
					contents: each
					type: #local) ] ].
	^ additionals
]

{ #category : #'*necompletion' }
Workspace >> completionAdditionalsFromOCompletion [
	"should spend more time to get it"
	
	bindings ifNil: [ ^#() ].
	^bindings keys asArray replace: [ :each | 
		NECLocalEntry 
			contents: each
			type: #local ]
]

{ #category : #'*necompletion' }
Workspace >> isCodeCompletionAllowed [
	^true

]
