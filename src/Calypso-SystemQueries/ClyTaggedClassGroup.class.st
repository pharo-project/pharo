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
	#name : #ClyTaggedClassGroup,
	#superclass : #ClyClassGroup,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #testing }
ClyTaggedClassGroup class >> isBasedOnClassTag [
	^true
]

{ #category : #testing }
ClyTaggedClassGroup class >> isEditableGroup [
	^true
]

{ #category : #'instance creation' }
ClyTaggedClassGroup class >> withClassesFrom: aPackageScope taggedBy: tagName [
	| classQuery |
	classQuery := ClyTaggedClassesQuery by: tagName from: aPackageScope.
	^ClyTaggedClassGroup named: tagName on: classQuery.
]

{ #category : #operations }
ClyTaggedClassGroup >> importClass: aClass [
	| newPackages |
	super importClass: aClass.
	
	newPackages := OrderedCollection new.
	classQuery scope packagesDo: [ :each | 
		each = aClass package ifFalse: [ newPackages add: each ]].
	newPackages size > 1 ifTrue: [ ^self error: 'You should select single package for import!' ].
	newPackages ifNotEmpty: [ newPackages first addClass: aClass]. 
	aClass tagWith: self tag
]

{ #category : #operations }
ClyTaggedClassGroup >> removeWithClasses [

	super removeWithClasses.
	classQuery scope packagesDo: [ :each |
		each removeClassTag: self tag]
]

{ #category : #operations }
ClyTaggedClassGroup >> renameClassTagTo: newTag [
	self classes do: [ :each |
		each tagWith: newTag.
		each untagFrom: self tag].
	
	classQuery scope packagesDo: [ :each | 
		(each tagsForClasses includes: newTag) 
			ifFalse: [ each addClassTag: newTag ].
		each removeClassTag: self tag]
]

{ #category : #accessing }
ClyTaggedClassGroup >> tag [
	^ classQuery tag
]
