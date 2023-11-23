"
I represent group of classes which does marked by specific tag.

You can create my instance using: 

	ClyTaggedClassGroup withClassesFrom: aPackageScope taggedBy: aSymbol.
	 
I provide simple method to rename this tag:

	aGroup renameClassTagTo: aSymbol

Which in fact untags all classes and then marks them with new tag.

For the #removeWithClasses operation I remove empty class tag from registrered tags of package.
	
My class query is ClyTaggedClasses
"
Class {
	#name : 'ClyTaggedClassGroup',
	#superclass : 'ClyClassGroup',
	#category : 'Calypso-SystemQueries-Domain',
	#package : 'Calypso-SystemQueries',
	#tag : 'Domain'
}

{ #category : 'testing' }
ClyTaggedClassGroup class >> isBasedOnPackageTag [
	^true
]

{ #category : 'testing' }
ClyTaggedClassGroup class >> isEditableGroup [
	^true
]

{ #category : 'instance creation' }
ClyTaggedClassGroup class >> withClassesFrom: aPackageScope taggedBy: tagName [
	| classQuery |
	classQuery := ClyTaggedClassesQuery by: tagName from: aPackageScope.
	^ClyTaggedClassGroup named: tagName on: classQuery
]

{ #category : 'operations' }
ClyTaggedClassGroup >> removeWithClasses [

	super removeWithClasses.
	classQuery scope packagesDo: [ :each | each removeTag: self tag ]
]

{ #category : 'operations' }
ClyTaggedClassGroup >> renameTagTo: newTag [

	self classes do: [ :class | class packageTag: newTag ].

	classQuery scope packagesDo: [ :package |
		package ensureTag: newTag.
		package removeTag: self tag ]
]

{ #category : 'accessing' }
ClyTaggedClassGroup >> tag [
	^ classQuery tag
]
