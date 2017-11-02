Extension { #name : #GLMSystemWindow }

{ #category : #'*GT-Playground' }
GLMSystemWindow >> acceptAction: anAction [ 
	"Be compatible with edit:label:accept:.
	Not the best place for this method.
	Interim situation to move things forward..."
	self flag: #TODO.
	self model ifNotNil: [:aModel | aModel acceptAction: anAction].
]

{ #category : #'*GT-Playground' }
GLMSystemWindow >> acceptContents: aString [ 
	"Be compatible with edit:label:accept:.
	Not the best place for this method.
	Interim situation to move things forward..."
	self flag: #TODO.
	self model ifNotNil: [:aModel | aModel acceptContents: aString].

	
]

{ #category : #'*GT-Playground' }
GLMSystemWindow >> model: anObject [
	model := anObject
]
