"
I represent a parameter in an expanded matrix.
I know how to update the test case with my value.
Also, I know how to print me when printing the test case. 
"
Class {
	#name : #ParametrizedTestExpandedParameter,
	#superclass : #Object,
	#instVars : [
		'selector',
		'valuable'
	],
	#category : #'ParametrizedTests-Core'
}

{ #category : #comparing }
ParametrizedTestExpandedParameter >> = anOption [

	^ self selector = anOption selector
		and: [ self valuable = anOption valuable ]
]

{ #category : #applying }
ParametrizedTestExpandedParameter >> applyTo: aTest [

	| anObject |
	anObject := self valuable isBlock
		ifTrue: [ self valuable cull: aTest ]
		ifFalse: [ self valuable ].

	aTest perform: selector asMutator with: anObject
]

{ #category : #converting }
ParametrizedTestExpandedParameter >> asTestParameter [

	^ self
]

{ #category : #comparing }
ParametrizedTestExpandedParameter >> hash [

	^ self selector hash + self valuable hash
]

{ #category : #accessing }
ParametrizedTestExpandedParameter >> key [
	self shouldBeImplemented.
]

{ #category : #printing }
ParametrizedTestExpandedParameter >> printOn: aStream [
	aStream print: (selector -> valuable)
]

{ #category : #accessing }
ParametrizedTestExpandedParameter >> selector [
	^ selector
]

{ #category : #accessing }
ParametrizedTestExpandedParameter >> selector: anObject [
	selector := anObject
]

{ #category : #accessing }
ParametrizedTestExpandedParameter >> suiteName [
	^ selector , ' -> ' , valuable printString
]

{ #category : #accessing }
ParametrizedTestExpandedParameter >> valuable [
	^ valuable
]

{ #category : #accessing }
ParametrizedTestExpandedParameter >> valuable: aValuable [ 
	valuable := aValuable
]
