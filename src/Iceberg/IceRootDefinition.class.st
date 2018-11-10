"
I am an IceDefinition representing the repository root. I contain all packages, directories and files of the repository. I'm not meant to be shown.

See my superclasses for more information.
"
Class {
	#name : #IceRootDefinition,
	#superclass : #IceDefinition,
	#category : #'Iceberg-Changes'
}

{ #category : #visiting }
IceRootDefinition >> accept: aVisitor [
	
	^ aVisitor visitRootDefinition: self
]

{ #category : #initialization }
IceRootDefinition >> initialize [
	super initialize.
	name := ''.
]

{ #category : #testing }
IceRootDefinition >> isRootDefinition [
	
	^ true
]
