"
I'm a simple example of fast table who will accept drops of classes.

FTExamples exampleDragAndDrop1.

"
Class {
	#name : #FTExampleDropDataSource,
	#superclass : #FTSimpleDataSource,
	#category : 'Morphic-Widgets-FastTable-Examples'
}

{ #category : #'drag and drop' }
FTExampleDropDataSource >> dropElements: aCollection index: rowIndex [
	self elements: (rowIndex = 0
		ifTrue: [ self elements, aCollection ]
		ifFalse: [ (self elements first: rowIndex), aCollection, (self elements allButFirst: rowIndex) ]).
	
]

{ #category : #initialization }
FTExampleDropDataSource >> initialize [ 
	super initialize.
	self elements: #()
]

{ #category : #'drag and drop' }
FTExampleDropDataSource >> wantsDropElements: aCollection type: type index: rowIndex [
	^ aCollection allSatisfy: #isClass
]
