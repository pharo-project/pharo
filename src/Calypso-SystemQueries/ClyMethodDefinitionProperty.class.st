"
I represent definition of method which includes defining package and defining class.

You can create me by: 

	ClyClassDefinitionProperty of: aMethod 
	
Also I provide extra flag #isExtension to mark that method is extension.
			 
Internal Representation and Key Implementation Points.

    Instance Variables
	isExtension:		<Boolean>
"
Class {
	#name : #ClyMethodDefinitionProperty,
	#superclass : #ClyItemDefinitionProperty,
	#instVars : [
		'isExtension'
	],
	#category : #'Calypso-SystemQueries-Properties'
}

{ #category : #'instance creation' }
ClyMethodDefinitionProperty class >> of: aMethod [
	| methodPackage methodClass |
	methodPackage := aMethod package.
	methodClass := aMethod origin.
	methodPackage ifNil: [ methodPackage := methodClass package ]. "it happens when creation new method in trait. It should be fixed in system"
	
	^self package: methodPackage class: methodClass 
]

{ #category : #'instance creation' }
ClyMethodDefinitionProperty class >> package: aPackage class: aClass [
	
	^(super package: aPackage class: aClass)
		isExtension: aClass package ~~ aPackage
]

{ #category : #accessing }
ClyMethodDefinitionProperty >> isExtension [
	^ isExtension
]

{ #category : #accessing }
ClyMethodDefinitionProperty >> isExtension: anObject [
	isExtension := anObject
]

{ #category : #printing }
ClyMethodDefinitionProperty >> printDefinition [

	^self printDefiningClass
]
