"
This is a strategy that populates a portSymbol with the result of evaluating valueBlock.
"
Class {
	#name : #GLMPortUpdater,
	#superclass : #Object,
	#instVars : [
		'portSymbol',
		'valueBlock'
	],
	#category : #'Glamour-Helpers'
}

{ #category : #accessing }
GLMPortUpdater >> glamourValueWithArgs: anArray [ 
	 
	(anArray first pane port: self portSymbol) 
		value: (self valueBlock glamourValueWithArgs: anArray)
]

{ #category : #accessing }
GLMPortUpdater >> portSymbol [ 
	 
	^portSymbol
]

{ #category : #accessing }
GLMPortUpdater >> portSymbol: anObject [ 
	 
	portSymbol := anObject
]

{ #category : #accessing }
GLMPortUpdater >> valueBlock [ 
	 
	^valueBlock
]

{ #category : #accessing }
GLMPortUpdater >> valueBlock: anObject [ 
	 
	valueBlock := anObject
]
