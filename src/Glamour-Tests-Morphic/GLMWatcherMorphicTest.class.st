Class {
	#name : #GLMWatcherMorphicTest,
	#superclass : #GLMMorphicTest,
	#category : 'Glamour-Tests-Morphic'
}

{ #category : #tests }
GLMWatcherMorphicTest >> expectedFailures [ 
	^ #(testSimple testWithDynamicPresentation)

]

{ #category : #tests }
GLMWatcherMorphicTest >> testSimple [
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit
		to: #one;
		andShow: [ :a | a list ].
	browser transmit
		from: #one;
		toWatcher;
		andShow: [ :a | a text ].
	window := browser openOn: (1 to: 42).
	((browser paneNamed: #one) port: #selection) value: 42.
	(Delay forMilliseconds: 1000) wait.
	self assert: (self find: RubScrolledTextMorph in: GLMWatcherWindow uniqueInstance) text equals: '42'
]

{ #category : #tests }
GLMWatcherMorphicTest >> testWithDynamicPresentation [
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit
		to: #one;
		andShow: [ :a | a list ].
	browser transmit
		from: #one;
		toWatcher;
		andShow: [ :a | a dynamic display: [ :x | GLMRubricTextPresentation new ] ].
	window := browser openOn: (1 to: 42).
	((browser paneNamed: #one) port: #selection) value: 42.
	(Delay forMilliseconds: 1000) wait.
	self assert: (self find: RubScrolledTextMorph in: GLMWatcherWindow uniqueInstance) text equals: '42'
]
