"
I represent the definition of an element in an Iceberg repository. My subclasses represent more concretely classes, traits, extensions, methods and files.

I am used to easily read and write from a repository working copy or commit. The Filetree or Tonel parser create IceDefinitions that are later on used to browse, generate diffs, or merges.

My main API is used to show myself in a diff or merge browser:

 - contents: textual value of the definition (e.g., the code of a class or a method, the contents of a file)
 - icon: the preferred icon to show this entity
 - accept: visitor API
 - is* to test which of my subclasses you're working with

I am also used for comparison to build diffs. See my definitions of equality along my subclasses.
"
Class {
	#name : #IceDefinition,
	#superclass : #Object,
	#instVars : [
		'name'
	],
	#category : #'Iceberg-Changes'
}

{ #category : #'instance creation' }
IceDefinition class >> named: aString [ 
	
	^ self new
		name: aString;
		yourself
]

{ #category : #comparing }
IceDefinition >> = anotherDefinition [

	^ self species = anotherDefinition species
		and: [ self name = anotherDefinition name ]
]

{ #category : #visiting }
IceDefinition >> accept: aVisitor [
	
	self subclassResponsibility
]

{ #category : #accessing }
IceDefinition >> contents [
	
	self subclassResponsibility
]

{ #category : #'morphic-compatibility' }
IceDefinition >> contextClass [
	
	^ nil
]

{ #category : #comparing }
IceDefinition >> hash [

	^ self name hash
]

{ #category : #testing }
IceDefinition >> isClassDefinition [
	
	^ false
]

{ #category : #testing }
IceDefinition >> isDirectoryDefinition [
	
	^ false
]

{ #category : #testing }
IceDefinition >> isExtensionDefinition [
	
	^ false
]

{ #category : #testing }
IceDefinition >> isFileDefinition [
	
	^ false
]

{ #category : #testing }
IceDefinition >> isMethodDefinition [
	
	^ false
]

{ #category : #testing }
IceDefinition >> isPackageDefinition [
	
	^ false
]

{ #category : #testing }
IceDefinition >> isRootDefinition [
	
	^ false
]

{ #category : #testing }
IceDefinition >> isTraitDefinition [
	
	^ false
]

{ #category : #accessing }
IceDefinition >> key [
	
	^ name
]

{ #category : #accessing }
IceDefinition >> name [
	
	^ name
]

{ #category : #accessing }
IceDefinition >> name: aString [ 
	
	name := aString
]

{ #category : #visiting }
IceDefinition >> removeFrom: aVisitor [
	
	self subclassResponsibility
]
