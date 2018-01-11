"
I'm a model for exposing method information.
I'm an example to be used in FTExampleMethodListDataSource and FTExampleMethodTableDataSource.
"
Class {
	#name : #FTExampleMethodModel,
	#superclass : #Object,
	#instVars : [
		'method',
		'actionIcon'
	],
	#category : #'Morphic-Widgets-FastTable-Examples'
}

{ #category : #'instance creation' }
FTExampleMethodModel class >> method: aMethod [
	^ self basicNew 
		initializeMethod: aMethod;
		yourself
]

{ #category : #'instance creation' }
FTExampleMethodModel class >> new [
	self error: 'Use #method:'
]

{ #category : #accessing }
FTExampleMethodModel >> actionOn: aBrowser [
	^ actionIcon ifNil: [ actionIcon := self createActionIconOn: aBrowser ]
]

{ #category : #factory }
FTExampleMethodModel >> createActionIconOn: aBrowser [
	^Smalltalk ui icons iconNamed: #scriptManagerIcon
]

{ #category : #initialization }
FTExampleMethodModel >> initializeMethod: aMethod [
	self initialize.
	method := aMethod
]

{ #category : #accessing }
FTExampleMethodModel >> method [
	^ method
]

{ #category : #accessing }
FTExampleMethodModel >> name [
	^ self method selector
]

{ #category : #accessing }
FTExampleMethodModel >> origin [
	^ self method origin
]

{ #category : #accessing }
FTExampleMethodModel >> protocol [
	^ self method protocol
]
