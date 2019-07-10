"
I am group of methods which reference some variable. 

I not keep this variable directly. All required information is passed to my instance from variables query
"
Class {
	#name : #ClyVariableMethodGroup,
	#superclass : #ClyMethodGroup,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #testing }
ClyVariableMethodGroup class >> isCalypsoItemType: aClass [
	 
	^self = aClass
]

{ #category : #testing }
ClyVariableMethodGroup class >> isEditableGroup [
	^true
]

{ #category : #'instance creation' }
ClyVariableMethodGroup class >> withMethodsFrom: aClassScope referencedVar: aVariable [
	^self 
		named: aVariable name 
		on: (ClyVariableReferencesQuery of: aVariable from: aClassScope)
]

{ #category : #accessing }
ClyVariableMethodGroup >> variable [
	^methodQuery variableQuery resultItems first "it should be always variable"
]
