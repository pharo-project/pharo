"
Revert work
"
Class {
	#name : 'GoferRevert',
	#superclass : 'GoferUpdate',
	#category : 'Gofer-Deprecated-Operations',
	#package : 'Gofer-Deprecated',
	#tag : 'Operations'
}

{ #category : 'running' }
GoferRevert >> execute [

	self workingCopies
		do: [ :each | each modified: false ].
	super execute
]

{ #category : 'private' }
GoferRevert >> referenceFor: aReference [

	| ancestors |
	ancestors := aReference workingCopy ancestry ancestors.
	ancestors isEmpty ifTrue: [ ^ MCSnapshot new ].
	^ GoferVersionReference name: ancestors first name
]
