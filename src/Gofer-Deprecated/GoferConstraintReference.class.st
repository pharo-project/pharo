"
A GoferPackageReference refers to the latest version of a Monticello package satisfying an additional constraint.
"
Class {
	#name : 'GoferConstraintReference',
	#superclass : 'GoferPackageReference',
	#instVars : [
		'constraintBlock'
	],
	#category : 'Gofer-Deprecated-References',
	#package : 'Gofer-Deprecated',
	#tag : 'References'
}

{ #category : 'instance creation' }
GoferConstraintReference class >> name: aString constraint: aBlock [

	^ self basicNew
		initializeName: aString constraint: aBlock;
		yourself
]

{ #category : 'initialization' }
GoferConstraintReference >> initializeName: aString constraint: aBlock [

	self initializeName: aString.
	constraintBlock := aBlock
]

{ #category : 'private' }
GoferConstraintReference >> matches: aResolvedReference [

	^ (super matches: aResolvedReference) and: [
		  constraintBlock value: aResolvedReference ]
]
