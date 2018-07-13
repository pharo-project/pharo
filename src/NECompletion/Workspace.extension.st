Extension { #name : #Workspace }

{ #category : #'*NECompletion' }
Workspace >> completionAdditionals [
	"return the local variable definition of the workspace as information for the completion algorithm."
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
Workspace >> isCodeCompletionAllowed [
	^true

]
