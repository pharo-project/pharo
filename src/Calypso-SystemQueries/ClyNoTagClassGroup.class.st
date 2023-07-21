"
I represent group of classes which does not marked by any tag.

I provide simple method to tag all my classes:

	aGroup renameClassTagTo: aSymbol
	
It is polymorphic to ClyTaggedClassGroup which performs actual tag renaming.

My class query is ClyRestUntaggedClasses 
"
Class {
	#name : #ClyNoTagClassGroup,
	#superclass : #ClyClassGroup,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #testing }
ClyNoTagClassGroup class >> isBasedOnClassTag [
	^true
]

{ #category : #accessing }
ClyNoTagClassGroup class >> priority [
	^10
]

{ #category : #operations }
ClyNoTagClassGroup >> importClass: aClass [

	| newPackages |
	super importClass: aClass.
	newPackages := OrderedCollection new.
	classQuery scope packagesDo: [ :each | each = aClass package ifFalse: [ newPackages add: each ] ].
	newPackages size > 1 ifTrue: [ ^ self error: 'You should select single package for import!' ].

	newPackages ifNotEmpty: [ ^ newPackages first addClass: aClass ].
	self flag: #package. "I'm not sure this next line even makes sens after moving the class from package. The tag should be removed automatically?"
	aClass removePackageTag
]

{ #category : #operations }
ClyNoTagClassGroup >> renameClassTagTo: newTag [

	self classes do: [ :class | class packageTag: newTag ]
]
