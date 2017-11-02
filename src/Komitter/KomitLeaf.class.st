Class {
	#name : #KomitLeaf,
	#superclass : #KomitNode,
	#category : 'Komitter-UI'
}

{ #category : #accessing }
KomitLeaf >> icon [
	self content added
		ifTrue: [ ^ self iconNamed: #changeAddIcon ].
	self content modified
		ifTrue: [ ^ self iconNamed: #changeUpdateIcon ].
	self content removed
		ifTrue: [ ^ self iconNamed: #changeRemoveIcon ].
	^ nil
]

{ #category : #initialization }
KomitLeaf >> initialize [

	super initialize.
	
	self children: [ #() ].
	self whenSelectedChanged: [ :aBoolean | self parentNode ifNotNil: [ :parent | parent checkChildren ] ]
]
