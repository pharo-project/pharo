"
I represent Class variable of class.
I am like ClassVariable but I include explicit information about defining class.

You can create my instance using:
	ClyClassVariable named: #DependentsFields definedIn: Object.
	ClyClassVariable on: aClassVariable definedIn: aClass
 
"
Class {
	#name : #ClyClassVariable,
	#superclass : #ClyVariable,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'instance creation' }
ClyClassVariable class >> named: aSymbol definedIn: aClass [
	^self 
		on: (aClass classVariableNamed: aSymbol)
		definedIn: aClass
]

{ #category : #queries }
ClyClassVariable class >> queryClass [
	^ClyAllClassVariablesQuery
]

{ #category : #accessing }
ClyClassVariable >> detectDefiningClassFrom: anUserClass [

	self detectDefiningClassFrom: anUserClass ifAbsent: [ 
		self error: 'Cant find the class defining variable' ]
]

{ #category : #accessing }
ClyClassVariable >> detectDefiningClassFrom: anUserClass ifAbsent: aBlock [

	(anUserClass instanceSide classVariables includes: actualVariable) ifTrue: [ 
		definingClass := anUserClass instanceSide.
		^self].
	
	anUserClass instanceSide sharedPools do: [ :each |
		self detectDefiningClassFrom: each ifAbsent: [].
		definingClass ifNotNil: [ ^self ] ].
	
	anUserClass isRootInEnvironment ifTrue: [ ^aBlock value ].
	
	self detectDefiningClassFrom: anUserClass superclass ifAbsent: aBlock
]

{ #category : #testing }
ClyClassVariable >> isAccessibleFrom: aClass [
	(aClass instanceSide includesSharedPoolNamed: definingClass name) ifTrue: [ ^true ].
		
	^aClass instanceSide = definingClass or: [ aClass instanceSide inheritsFrom: definingClass ]
]

{ #category : #testing }
ClyClassVariable >> isClassVariable [
	^true
]

{ #category : #testing }
ClyClassVariable >> isLive [

	^true
]

{ #category : #operations }
ClyClassVariable >> openInspector [

	actualVariable value inspect
]
