Class {
	#name : #GLMTabulatorMorphicTest,
	#superclass : #GLMMorphicTest,
	#category : #'Glamour-Tests-Morphic'
}

{ #category : #tests }
GLMTabulatorMorphicTest >> testMultipleInitialSelection [
	| browser firstTreeMorph |
	browser := GLMTabulator new.
	browser column: #one.
	browser
		transmit to: #one;
		andShow: [ :a | 
					(a tree)
						beMultiple;
						display: [ :x | 1 to: x * 3 ] ].
	window := browser openOn: 5.
	(browser panes first port: #selection) value: #(2 5 3).
	firstTreeMorph := self find: MorphTreeMorph in: window.
	self assert: firstTreeMorph model selection notNil.
	self assert: firstTreeMorph model selection selectedItemOrItemsOrNil size equals: 3.
]

{ #category : #tests }
GLMTabulatorMorphicTest >> testSpawnTabulator [
	| browser |
	browser := GLMTabulator new.
	window := browser openOn: ($a to: $d).
	self assert: window model identicalTo: browser.

	self waitUntilUIRedrawed.

	self assert: window submorphs last submorphs last submorphs last submorphs last class equals: GLMPaginatedMorphTreeMorph
]

{ #category : #tests }
GLMTabulatorMorphicTest >> testStatusbar [
	| browser statusMorph |
	browser := GLMTabulator new.
	browser addStatusbar.
	browser row: #one.
	browser transmit to: #one; andShow: [:a | a list display: #(1 2 3)].
	window := browser openOn: 42.
	((browser paneNamed: #one) port: #status) value: 2.

	self waitUntilUIRedrawed.

	statusMorph := (window submorphs last: 2) first submorphs first.	
	self assert: (statusMorph isKindOf: LabelMorph)
]
