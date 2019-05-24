"
I represent a glopbal variable.
I am like GlobalVariable.

You can create my instance using:
	ClyGlobalVariable on: aGlobalVariable
 
"
Class {
	#name : #ClyGlobalVariable,
	#superclass : #ClyVariable,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'instance creation' }
ClyGlobalVariable class >> on: aGlobalVariable [
	^self on: aGlobalVariable definedIn: nil
]

{ #category : #accessing }
ClyGlobalVariable >> detectDefiningClassFrom: anUserClass [
	"There is no defining class for the global"
]

{ #category : #testing }
ClyGlobalVariable >> isAccessibleFrom: aClass [

	^true
]

{ #category : #testing }
ClyGlobalVariable >> isClassVariable [
	^false
]

{ #category : #testing }
ClyGlobalVariable >> isLive [

	^true
]

{ #category : #operations }
ClyGlobalVariable >> openBrowser [

	actualVariable value browse
]

{ #category : #operations }
ClyGlobalVariable >> openInspector [

	actualVariable value inspect
]
