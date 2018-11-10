"
I return all methods which reference one of the variables retrieved by given variableQuery.

To create my instance use following expressions: 

	ClyVariableReferences toVariablesFrom: aVariableQuery.
	ClyVariableReferences from: aScope toVariablesFrom: aVariableQuery.

Query argument here is supposed return variables, instances of ClyVariable subclasses. So it can be complex query instead of concrete variable query.
  
And for simple cases there is another set of expressions when you need references to existing list of variables: 
	
	ClyVariableReferences of: aVariable.
	ClyVariableReferences of: aVariable from: aScope.
	
	ClyVariableReferences ofAny: { 
		ClyInstanceVariable named: #x definedIn: Point.
		ClyInstanceVariable named: #y definedIn: Point}.
	ClyVariableReferences ofAny: variables from: aScope.

Variables here are represented by subclasses of ClyVariable. 
		
I have few subclases which define specific meaning of reference like only readers or only writers. They redefine method #doesMethod:useVar: for this purpose.

Internal Representation and Key Implementation Points.

    Instance Variables
	variableQuery:		<ClyVariableQuery>
"
Class {
	#name : #ClyVariableReferences,
	#superclass : #ClyMethodQuery,
	#instVars : [
		'variableQuery'
	],
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #'instance variables' }
ClyVariableReferences class >> from: aScope toVariablesFrom: aVariableQuery [ 
	^(self from: aScope)
		variableQuery: aVariableQuery 
]

{ #category : #'instance variables' }
ClyVariableReferences class >> of: aVariable [
	^self ofAny: { aVariable }
]

{ #category : #'instance variables' }
ClyVariableReferences class >> of: aVariable from: aScope [
	^self ofAny: { aVariable } from: aScope
]

{ #category : #'instance variables' }
ClyVariableReferences class >> ofAny: variables [
	^self new
		variableQuery: (ClyConstantQuery returning: variables)
]

{ #category : #'instance variables' }
ClyVariableReferences class >> ofAny: variables from: aScope [
	^aScope adoptQuery: (self toVariablesFrom: (ClyConstantQuery returning: variables))
]

{ #category : #'instance variables' }
ClyVariableReferences class >> toVariablesFrom: aVariableQuery [ 
	^self new
		variableQuery: aVariableQuery 
]

{ #category : #comparing }
ClyVariableReferences >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^ false ].
	^ variableQuery = anObject variableQuery
]

{ #category : #execution }
ClyVariableReferences >> buildResult: aQueryResult [
	| filteredMethods vars |
	vars := variableQuery execute items.
	filteredMethods := OrderedCollection new.
	
	scope methodsDo: [ :each | 
		(self doesMethod: each useAnyOf: vars) ifTrue: [filteredMethods add: each]].
	
	aQueryResult fillWith: filteredMethods
]

{ #category : #printing }
ClyVariableReferences >> description [

	^'references to ', self printVariables 
]

{ #category : #testing }
ClyVariableReferences >> doesMethod: aMethod useAnyOf: vars [
	
	^vars anySatisfy: [ :each |
		self doesMethod: aMethod useVar: each]
]

{ #category : #testing }
ClyVariableReferences >> doesMethod: aMethod useVar: aVariable [

	^aVariable isReferencedIn: aMethod
]

{ #category : #comparing }
ClyVariableReferences >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ super hash bitXor: variableQuery hash
]

{ #category : #printing }
ClyVariableReferences >> printExtraInfoOn: aStream [
	super printExtraInfoOn: aStream.

	aStream nextPutAll: self printVariables 
]

{ #category : #printing }
ClyVariableReferences >> printVariables [

	^variableQuery scope == scope 
		ifTrue: [variableQuery description]
		ifFalse: [ variableQuery fullDescription ]
]

{ #category : #testing }
ClyVariableReferences >> selectsMethod: aMethod [
	
	| vars |
	vars := variableQuery execute.
	
	^self doesMethod: aMethod useAnyOf: vars items
]

{ #category : #accessing }
ClyVariableReferences >> variableQuery [
	^ variableQuery
]

{ #category : #accessing }
ClyVariableReferences >> variableQuery: aVariableQuery [
	variableQuery := aVariableQuery 
]
