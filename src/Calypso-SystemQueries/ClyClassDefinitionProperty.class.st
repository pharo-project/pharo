"
I represent definition of class which includes package and class itself as defining objects.

You can create me by: 

	ClyClassDefinitionProperty of: aClass
"
Class {
	#name : #ClyClassDefinitionProperty,
	#superclass : #ClyItemDefinitionProperty,
	#category : #'Calypso-SystemQueries-Properties'
}

{ #category : #'instance creation' }
ClyClassDefinitionProperty class >> of: aClass [
	
	^self package: aClass package class: aClass
]

{ #category : #printing }
ClyClassDefinitionProperty >> printDefinition [
	^definingPackageItem name
]
