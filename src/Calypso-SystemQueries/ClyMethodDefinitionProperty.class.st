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
	#name : 'ClyMethodDefinitionProperty',
	#superclass : 'ClyItemDefinitionProperty',
	#instVars : [
		'isExtension',
		'protocol'
	],
	#category : 'Calypso-SystemQueries-Properties',
	#package : 'Calypso-SystemQueries',
	#tag : 'Properties'
}

{ #category : 'instance creation' }
ClyMethodDefinitionProperty class >> of: aMethod [
	"Answer a <ClyMethodDefinitionProperty>."

	| methodPackage methodClass |
	methodClass := aMethod origin.
	methodPackage := aMethod package ifNil: [ methodClass package ]. "it happens when creation new method in trait. It should be fixed in system"

	^ self package: methodPackage class: methodClass protocol: aMethod protocolName
]

{ #category : 'instance creation' }
ClyMethodDefinitionProperty class >> package: aPackage class: aClass [

	^(super package: aPackage class: aClass)
		isExtension: aClass package ~~ aPackage
]

{ #category : 'instance creation' }
ClyMethodDefinitionProperty class >> package: aPackage class: aClass protocol: aProtocol [

	^(super package: aPackage class: aClass)
		isExtension: aClass package ~~ aPackage;
		protocol: aProtocol;
		yourself
]

{ #category : 'accessing' }
ClyMethodDefinitionProperty >> isExtension [
	^ isExtension
]

{ #category : 'accessing' }
ClyMethodDefinitionProperty >> isExtension: anObject [
	isExtension := anObject
]

{ #category : 'printing' }
ClyMethodDefinitionProperty >> printDefiningProtocol [
	^ protocol
]

{ #category : 'printing' }
ClyMethodDefinitionProperty >> printDefinition [

	^self printDefiningClass
]

{ #category : 'accessing' }
ClyMethodDefinitionProperty >> protocol: anObject [
	protocol := anObject
]
