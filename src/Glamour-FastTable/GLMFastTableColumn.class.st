"
I'm a glamour column for fast tables.
"
Class {
	#name : #GLMFastTableColumn,
	#superclass : #FTColumn,
	#instVars : [
		'glamourColumn',
		'target',
		'selector',
		'arguments'
	],
	#category : 'Glamour-FastTable-Model'
}

{ #category : #execute }
GLMFastTableColumn >> execute [ 
	target ifNil: [ ^ self ].
	selector ifNil: [ ^ self ].
	
	target perform: selector withArguments: arguments
]

{ #category : #accessing }
GLMFastTableColumn >> glamourColumn [
	^ glamourColumn
]

{ #category : #accessing }
GLMFastTableColumn >> glamourColumn: anObject [
	glamourColumn := anObject
]

{ #category : #accessing }
GLMFastTableColumn >> target: aTarget actionSelector: aSelector arguments: aCollection [ 
	target := aTarget. 
	selector := aSelector.
	arguments := aCollection
]
