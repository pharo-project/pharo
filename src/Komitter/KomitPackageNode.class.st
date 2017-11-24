"
I am a node in the Komit tree representing a package
"
Class {
	#name : #KomitPackageNode,
	#superclass : #KomitNode,
	#category : #'Komitter-UI'
}

{ #category : #private }
KomitPackageNode >> getClasses [

	^ self content classes
]

{ #category : #api }
KomitPackageNode >> icon [
	^ self iconNamed: #dirtyPackageIcon
]

{ #category : #initialization }
KomitPackageNode >> initialize [
	super initialize.
	self icon: (self iconNamed: #dirtyPackageIcon).
	self
		whenSelectedChanged: [ :aBoolean | 
			self
				ifNotSilentDo: [ aBoolean
						ifTrue: [ self selectAllChildren ]
						ifFalse: [ self unselectAllChildren ] ] ].
	self
		whenContentChanged: [ :c | 
			| childrenNode |
			childrenNode := self getClasses
				collect: [ :each | 
					KomitClassNode new
						content: each;
						yourself ].
			self children: [ childrenNode ] ].
	self hasChildren: [ self isEmpty not ]
]

{ #category : #testing }
KomitPackageNode >> isEmpty [

	^ self content patch operations isEmpty
]

{ #category : #api }
KomitPackageNode >> label [

	^ self content package name
]

{ #category : #printing }
KomitPackageNode >> printOn: aStream [

	super printOn: aStream.
	aStream << '[ '.
	self content package name printOn: aStream.
	aStream << ' ]'.
]
