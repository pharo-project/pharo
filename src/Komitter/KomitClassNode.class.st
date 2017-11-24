"
I am a node in the Komit tree representing a class
"
Class {
	#name : #KomitClassNode,
	#superclass : #KomitNode,
	#classInstVars : [
		'deletedClassIcon',
		'modifiedClassIcon',
		'addedClassIcon'
	],
	#category : #'Komitter-UI'
}

{ #category : #icons }
KomitClassNode class >> addedClassIcon [
	^ addedClassIcon
		ifNil: [ addedClassIcon := (self defaultClassIcon asFormOfDepth: 32)
				mergeBottomRightWith: (self iconNamed: #overlayAdd) ]
]

{ #category : #icons }
KomitClassNode class >> defaultClassIcon [
	^ self iconNamed: #classIcon
]

{ #category : #icons }
KomitClassNode class >> deletedClassIcon [
	^ deletedClassIcon
		ifNil: [ deletedClassIcon := (self defaultClassIcon asFormOfDepth: 32)
				mergeBottomRightWith: (self iconNamed: #overlayRemove) ]
]

{ #category : #icons }
KomitClassNode class >> modifiedClassIcon [
	^ modifiedClassIcon
		ifNil: [ modifiedClassIcon := (self defaultClassIcon asFormOfDepth: 32)
				mergeBottomRightWith: (self iconNamed: #overlayModification) ]
]

{ #category : #private }
KomitClassNode >> getDefinitions [

	^ self content definitions
]

{ #category : #private }
KomitClassNode >> getMethods [

	^ self content methods
]

{ #category : #accessing }
KomitClassNode >> icon [
	
	self content added ifTrue: [ ^ self class addedClassIcon ].
	self content modified ifTrue: [ ^ self class modifiedClassIcon ].
	self content removed ifTrue: [ ^ self class deletedClassIcon ].
	
	^ self class defaultClassIcon
]

{ #category : #initialization }
KomitClassNode >> initialize [	
	super initialize.
	
	self whenSelectedChanged: [ :aBoolean |
		self parentNode ifNotNil: [ :parent | parent checkChildren ].
		self ifNotSilentDo: [
			aBoolean
				ifTrue: [ self selectAllChildren ]
				ifFalse: [ self unselectAllChildren ] ] ].
		
	self whenContentChanged: [ :c || childrenNode |
		childrenNode := self getDefinitions 
			collect: [ :each | KomitDefinitionNode new
				content: each;
				yourself ].
			childrenNode addAll: (self getMethods
				collect: [ :each | KomitMethodNode new
					content: each;
					yourself ]).
		self children: [ childrenNode ] ]
]

{ #category : #api }
KomitClassNode >> label [

	^ self content trackedClass
]

{ #category : #printing }
KomitClassNode >> printOn: aStream [

	super printOn: aStream.
	aStream << '[ '.
	self content trackedClass printOn: aStream.
	aStream << ' ]'.
]
