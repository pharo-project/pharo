"
I am group of methods which reference any instance variable of some class.
I not keep this class directly. All required information is passed to my instance from variables query
"
Class {
	#name : #ClyVarDefinitionMethodGroup,
	#superclass : #ClyMethodGroup,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'instance creation' }
ClyVarDefinitionMethodGroup class >> withMethodsFrom: aClassScope referencedAny: aVariableClass definedIn: definingClass [

	| variableQuery |
	variableQuery := aVariableClass queryClass 
		from: (aClassScope asScope: ClyClassScope of: definingClass).
	^self 
		named: definingClass name 
		on: (ClyVariableReferencesQuery from: aClassScope toVariablesFrom: variableQuery)
]
