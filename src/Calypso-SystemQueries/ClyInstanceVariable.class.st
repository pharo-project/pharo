"
I represent instance variable of class.
I am like slot but include explicit information about defining class.

You can create my instance using:
	ClyInstanceVariable named: #x definedIn: Point.
	ClyInstanceVariable on: aSlot definedIn: aClass
"
Class {
	#name : #ClyInstanceVariable,
	#superclass : #ClyVariable,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'instance creation' }
ClyInstanceVariable class >> named: aSymbol definedIn: aClass [
	^self 
		on: (aClass slotNamed: aSymbol)
		definedIn: aClass
]

{ #category : #queries }
ClyInstanceVariable class >> queryClass [
	^ClyAllInstanceVariablesQuery
]

{ #category : #accessing }
ClyInstanceVariable >> detectDefiningClassFrom: anUserClass [

	(anUserClass slots includes: actualVariable) ifTrue: [ 
		definingClass := anUserClass.
		^self].
	
	anUserClass isRootInEnvironment ifTrue: [ 
		self error: 'Cant find the class defining variable' ].
	
	self detectDefiningClassFrom: anUserClass superclass
]

{ #category : #testing }
ClyInstanceVariable >> isAccessibleFrom: aClass [

	^aClass = definingClass or: [ aClass inheritsFrom: definingClass ]
]

{ #category : #testing }
ClyInstanceVariable >> isClassVariable [
	^false
]

{ #category : #testing }
ClyInstanceVariable >> isLive [

	^definingClass isClassSide
]

{ #category : #operations }
ClyInstanceVariable >> openInspector [
	definingClass isClassSide ifTrue: [ 
		^(actualVariable read: definingClass instanceSide) inspect].
	
	super openInspector.
]
