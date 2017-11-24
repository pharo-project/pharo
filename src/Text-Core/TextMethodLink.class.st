"
An active link in a text linking to a method
"
Class {
	#name : #TextMethodLink,
	#superclass : #TextLink,
	#instVars : [
		'selector',
		'browseSenders',
		'sourceNode'
	],
	#category : #'Text-Core'
}

{ #category : #'instance creation' }
TextMethodLink class >> selector: aStringOrSymbol [
	^ self new
		selector: aStringOrSymbol;
		yourself
]

{ #category : #'instance creation' }
TextMethodLink class >> selector: aString senders: browseSenders [
	^ (self selector: aString)
		senders: browseSenders;
		yourself
]

{ #category : #'instance creation' }
TextMethodLink class >> sourceNode: aMessageNode [
	^ self new
		selector: aMessageNode selector;
		sourceNode: aMessageNode 
		yourself
]

{ #category : #comparing }
TextMethodLink >> = anotherObject [
	^ super = anotherObject
		and: [ selector = anotherObject selector
				and: [ browseSenders = anotherObject browseSenders
						and: [ self sourceNodeInterval = anotherObject sourceNodeInterval ] ] ]
]

{ #category : #accessing }
TextMethodLink >> browseSenders [

	^browseSenders 
]

{ #category : #comparing }
TextMethodLink >> hash [
	^ (((super hash + selector hash) hashMultiply + browseSenders hash) hashMultiply
		+ self sourceNodeInterval hash) hashMultiply
]

{ #category : #initialization }
TextMethodLink >> initialize [ 
	super initialize.
	browseSenders := false.
]

{ #category : #accessing }
TextMethodLink >> selector [
	^ selector
]

{ #category : #accessing }
TextMethodLink >> selector: aStringOrSymbol [
	selector := aStringOrSymbol asSymbol.
]

{ #category : #accessing }
TextMethodLink >> senders: aBoolean [
	browseSenders := aBoolean
]

{ #category : #accessing }
TextMethodLink >> sourceNode [
	^ sourceNode
]

{ #category : #accessing }
TextMethodLink >> sourceNode: anObject [
	sourceNode := anObject
]

{ #category : #private }
TextMethodLink >> sourceNodeInterval [
	sourceNode ifNil: [ ^ 1 to: 0 ].
	^ sourceNode keywordsPositions first to: sourceNode stop
]
