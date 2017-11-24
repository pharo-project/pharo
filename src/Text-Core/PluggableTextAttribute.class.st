"
An attribute which evaluates an arbitrary block when it is selected.
"
Class {
	#name : #PluggableTextAttribute,
	#superclass : #TextAction,
	#instVars : [
		'evalBlock'
	],
	#category : #'Text-Core'
}

{ #category : #'instance creation' }
PluggableTextAttribute class >> evalBlock: aBlock [
	^super new evalBlock: aBlock
]

{ #category : #evaluating }
PluggableTextAttribute >> actOnClick: anEvent for: anObject in: paragraph editor: editor [

	evalBlock ifNil: [ ^self ].
	evalBlock numArgs = 0 ifTrue: [ evalBlock value.  ^true ].
	evalBlock numArgs = 1 ifTrue: [ evalBlock value: anObject.  ^true ].
	self error: 'evalBlock should have 0 or 1 arguments'
]

{ #category : #accessing }
PluggableTextAttribute >> evalBlock: aBlock [
	evalBlock := aBlock
]
