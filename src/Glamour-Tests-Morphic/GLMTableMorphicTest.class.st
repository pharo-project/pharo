Class {
	#name : #GLMTableMorphicTest,
	#superclass : #GLMMorphicTest,
	#category : #'Glamour-Tests-Morphic'
}

{ #category : #tests }
GLMTableMorphicTest >> testColumnBlockTakesEntity [

	| browser receivedEntity |
	browser := GLMFinder new.
	receivedEntity := nil.
	browser show: [:a | a table
		column: 'Even' evaluated: [:each :entity | receivedEntity := entity. 'x']].
	window := browser openOn: (1 to: 5).
	World doOneCycle.
	self repeatAssert: [receivedEntity] equals: (1 to: 5).

]

{ #category : #tests }
GLMTableMorphicTest >> testColumnLabel [
	| browser tableMorph |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					(a table)
						display: [ :x | x - 3 to: x ];
						column: 'Column' evaluated: [ :x | x asString ];
						column: [ :x | 'Number ' , x asString ] evaluated: #asString ].
	window := browser openOn: 4.
	tableMorph := self find: MorphTreeMorph in: window.
	self assert: (tableMorph model roots collect: #item) equals: #(1 2 3 4) asOrderedCollection.
	self assert: tableMorph columns first header label equals: 'Column'.
	self assert: tableMorph columns last header label equals: 'Number 4'
]
