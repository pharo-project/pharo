"
A GLMBrickListAdaptor is scriptable implementation of adaptor that is mostly used with list scroll brick

Instance Variables
	countLogic:		<Object>
	morphLogic:		<Object>
	origin:		<Object>

countLogic
	- xxxxx

morphLogic
	- xxxxx

origin
	- xxxxx

"
Class {
	#name : #GLMBrickListModel,
	#superclass : #GLMBrickAbstractModel,
	#instVars : [
		'origin',
		'countLogic',
		'morphLogic'
	],
	#category : #'Glamour-Morphic-Brick-Model'
}

{ #category : #'instance creation' }
GLMBrickListModel class >> on: anObject [

	^ self new on: anObject
]

{ #category : #'adaptor api' }
GLMBrickListModel >> count [

	^ countLogic
		ifNil: [ origin size ]
		ifNotNil: [ countLogic cull: origin ] 
]

{ #category : #'adaptor api' }
GLMBrickListModel >> morphAt: anInteger convert: aMorph [

	^ morphLogic
		ifNil: [ (origin at: anInteger) asMorph ]
		ifNotNil: [
			morphLogic isSymbol
				ifTrue: [ origin perform: morphLogic withEnoughArguments: { anInteger . aMorph }  ]
				ifFalse: [ morphLogic cull: anInteger cull: aMorph ] ]
]

{ #category : #'instance creation' }
GLMBrickListModel >> morphAtLogic: aBlockWithTwoParameters [

	morphLogic := aBlockWithTwoParameters
]

{ #category : #'instance creation' }
GLMBrickListModel >> on: anObject [

	origin := anObject
]

{ #category : #accessing }
GLMBrickListModel >> origin [

	^ origin
]

{ #category : #'instance creation' }
GLMBrickListModel >> sizeLogic: aBlockWithNoParameters [

	countLogic := aBlockWithNoParameters
]
