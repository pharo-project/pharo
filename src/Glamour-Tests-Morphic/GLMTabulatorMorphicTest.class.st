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
GLMTabulatorMorphicTest >> testSizeAndSpan [
	"combining span (=fractional width/height) and size (=fixed width/height) did not work correctly before.
	2 is added in height comparisons because the layout code leaves one pixel offset on top and bottom."
	| browser panes heights |
	browser := GLMTabulator new.
	browser row: #top span: 2.
	browser row: #middle size: 80.
	browser row: #bottom.
	browser transmit to: #top; transformation: #first; andShow: [ :p | p text ].
	browser transmit to: #middle; transformation: #second; andShow: [ :p | p text ].
	browser transmit to: #bottom; transformation: #last; andShow: [ :p | p text ].
	window := browser openOn: #('Should be scaled at 2/3' 'Should be fixed' 'Should be scaled at 1/3').
	{200@500. 100@300} do: [:ext |
		window extent: ext.
		World doOneCycle.
		panes := window submorphs last submorphs last: 3.
		heights := panes collect: [ :each | each height+2 ].
		self assert: panes first owner height equals: heights sum.
		self assert: panes last bottom + 1 equals: panes first owner bottom.
		self assert: heights second equals: 80.
		self assert: (heights first - (heights sum - 80 / 3 * 2)) abs <= 1.
		self assert: (heights last - (heights sum - 80 / 3)) abs <= 1]
]

{ #category : #tests }
GLMTabulatorMorphicTest >> testSpawnTabulator [

	| browser |
	browser := GLMTabulator new.
	window := browser openOn: ($a to: $d).
	self assert: window model == browser.
	self repeatAssert: [
		window submorphs last 
					submorphs last 
						submorphs last 
							submorphs last class] equals: GLMPaginatedMorphTreeMorph

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
	World doOneCycle.
	statusMorph := (window submorphs last: 2) first submorphs first.	
	self assert: (statusMorph isKindOf: LabelMorph)
]
