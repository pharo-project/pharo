"
I return methods which reference given classes.
So I expect that my variableQuery will be in fact class query.
But because class itself is not a variable I convert given query to class bindings which can play role of variables in the method filtering logic:

	ClyClassReferences>>variableQuery: aClassQuery
		super variableQuery: (aClassQuery withResult: ClyClassBindings new)

And in addition I provide more readable methods to instantiate my instances from classes:

	ClyClassReferences to: aClass.
	ClyClassReferences to: aClass from: aScope.
	ClyClassReferences toAny: {Array. String}.
	ClyClassReferences toAny: {Array. String} from: aScope.
"
Class {
	#name : #ClyClassReferences,
	#superclass : #ClyVariableReferences,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #'instance creation' }
ClyClassReferences class >> to: aClass [

	^self of: aClass
]

{ #category : #'instance creation' }
ClyClassReferences class >> to: aClass from: aScope [

	^self of: aClass from: aScope
]

{ #category : #'instance creation' }
ClyClassReferences class >> toAny: classes [

	^self ofAny: classes
]

{ #category : #'instance creation' }
ClyClassReferences class >> toAny: classes from: aScope [

	^self ofAny: classes from: aScope
]

{ #category : #testing }
ClyClassReferences >> doesMethod: aMethod useVar: aVariable [
	(super doesMethod: aMethod useVar: aVariable) ifTrue: [ ^true ].

	^aMethod hasSelector: aVariable name asSymbol
]

{ #category : #accessing }
ClyClassReferences >> variableQuery: aClassQuery [
	super variableQuery: (aClassQuery withResult: ClyClassBindings new)
]
