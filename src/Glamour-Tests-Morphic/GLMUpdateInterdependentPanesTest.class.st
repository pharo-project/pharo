Class {
	#name : #GLMUpdateInterdependentPanesTest,
	#superclass : #GLMMorphicTest,
	#category : #Glamour-Tests-Morphic
}

{ #category : #tests }
GLMUpdateInterdependentPanesTest >> testAllowNil [
	| browser detailsMorph |
	browser := GLMTabulator new.
	browser
		column: [ :c | 
					c
						row: #one;
						row: #two ];
		column: #details.
	browser
		transmit to: #one;
		andShow: [ :a | a tree display: #(1 2 3) ].
	browser
		transmit to: #two;
		andShow: [ :a | a tree display: #(#a #b #c) ].
	browser
		transmit to: #details;
		from: #one;
		from: #two;
		andShow: [ :a | 
					(a text)
						display: [ :one :two | one asString , two asString ];
						allowNil ].
	window := browser openOn: 42.
	((browser paneNamed: #one) port: #selection) value: 2.
	detailsMorph := window submorphs last submorphs last submorphs first.
	self assert: detailsMorph text asString equals: '2nil'.
	((browser paneNamed: #two) port: #selection) value: #c.
	detailsMorph := window submorphs last submorphs last submorphs first.
	self assert: detailsMorph text asString equals: '2c'.
	((browser paneNamed: #one) port: #selection) value: nil.
	detailsMorph := window submorphs last submorphs last submorphs first.
	self assert: detailsMorph text asString equals: 'nilc'
]

{ #category : #tests }
GLMUpdateInterdependentPanesTest >> testInterdependentPanesShouldUpdateTheFocusTriggerPane [
	|  treeMorphThree treeMorphThree2 browser treeMorphTwo treeMorphOne |
	browser := GLMTabulator new.
	browser column: #one; column: #two; column: #three.
	(browser transmit)
		to: #one;
		andShow: [ :a | a list display: [ :x | 1 to: x ] ].
	(browser transmit)
		to: #two; from: #one;
		andShow: [ :a | a list display: [ :x | 1 to: x * 2 ] ].
	(browser transmit)
		to: #three; from: #two;
		andShow: [ :a | a list display: [ :x | 1 to: x * 3 ] ].
	browser transmit toOutsidePort: #focus; from: #three port: #focus.
	browser transmit to: #one port: #selection; fromOutsidePort: #focus; transformed: [ :x | x + 1 ].
	browser transmit to: #two port: #selection; fromOutsidePort: #focus; transformed: [ :x | x + 2 ].

	window := browser openOn: 5.
	((browser paneNamed: #one) port: #selection) value: 3.
	((browser paneNamed: #two) port: #selection) value: 6.
	treeMorphThree := self findWherePresentationIs: (browser paneNamed: #three) presentations first in: window.

	((browser paneNamed: #three) port: #selection) value: 2.
	((browser paneNamed: #three) port: #focus) value: 1.

	self assert: ((browser paneNamed: #one) port: #entity) value equals: 5.
	self assert: ((browser paneNamed: #one) port: #selection) value equals: 2.
	self assert: ((browser paneNamed: #two) port: #entity) value equals: 2.
	self assert: ((browser paneNamed: #two) port: #selection) value equals: 3.
	self assert: ((browser paneNamed: #three) port: #entity) value equals: 3.
	self assert: ((browser paneNamed: #three) port: #selection) value isNil.

	treeMorphThree2 := self findWherePresentationIs: (browser paneNamed: #three) presentations first in: window.
	self assert: treeMorphThree ~= treeMorphThree2.
	self assert: treeMorphThree2 selectedMorph isNil.

	treeMorphOne := self findWherePresentationIs: (browser paneNamed: #one) presentations first in: window.
	self assert: treeMorphOne selectedMorph notNil.
	
	treeMorphTwo := self findWherePresentationIs: (browser paneNamed: #two) presentations first in: window.
	self assert: treeMorphTwo selectedMorph notNil.

]

{ #category : #tests }
GLMUpdateInterdependentPanesTest >> testInterdependentPanesShouldUpdateTheFocusTriggerPaneWithText [
	| browser treeMorphThree treeMorphThree2 |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two;
		column: #three.
	(browser transmit)
		to: #one;
		andShow: [ :a | a text display: [ :x | 1 to: x ] ].
	(browser transmit)
		to: #two;
		from: #one;
		andShow: [ :a | a text display: [ :x | 1 to: x * 2 ] ].
	(browser transmit)
		to: #three;
		from: #two;
		andShow: [ :a | a text display: [ :x | 1 to: x * 3 ] ].
	(browser transmit)
		toOutsidePort: #focus;
		from: #three port: #focus.
	(browser transmit)
		to: #one port: #selection;
		fromOutsidePort: #focus;
		transformed: [ :x | x + 1 ].
	(browser transmit)
		to: #two port: #selection;
		fromOutsidePort: #focus;
		transformed: [ :x | x + 2 ].
	window := browser openOn: 5.
	((browser paneNamed: #one) port: #selection) value: 3.
	((browser paneNamed: #two) port: #selection) value: 6.
	treeMorphThree := (window submorphs last submorphs at: 5) submorphs first submorphs first.
	((browser paneNamed: #three) port: #selection) value: 2.
	((browser paneNamed: #three) port: #focus) value: 1.
	self assert: ((browser paneNamed: #one) port: #entity) value equals: 5.
	self assert: ((browser paneNamed: #one) port: #selection) value equals: 2.
	self assert: ((browser paneNamed: #two) port: #entity) value equals: 2.
	self assert: ((browser paneNamed: #two) port: #selection) value equals: 3.
	self assert: ((browser paneNamed: #three) port: #entity) value equals: 3.
	self assert: ((browser paneNamed: #three) port: #selection) value isNil.
	treeMorphThree2 := (window submorphs last submorphs at: 5) submorphs first submorphs first.
	self assert: treeMorphThree ~= treeMorphThree2
]

{ #category : #tests }
GLMUpdateInterdependentPanesTest >> testOnePanesShouldUpdateTheRenderedSelectionOnTheTriggerPane [

	| browser morphOne |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit to: #one; andShow: [:a | a list display: [:x | 1 to: x]].
	browser transmit from: #one port: #strongSelection; toOutsidePort: #toSelect.
	browser transmit fromOutsidePort: #toSelect; to: #one port: #selection; transformed: [:x | x + 1].
	window := browser openOn: 100.
	(browser panes first port: #strongSelection) value: 41.
	self assert: ((browser paneNamed: #one) port: #selection) value equals: 42.
	.
	morphOne := self findWherePresentationIs: (browser paneNamed: #one) presentations first in: window.
	self assert: morphOne notNil.
	self assert: morphOne selectedMorph notNil
]

{ #category : #tests }
GLMUpdateInterdependentPanesTest >> testTwoInterdependentPanesShouldUpdateTheRenderedSelectionOnTheTriggerPane [

	| browser morphTwo morphOne |
	browser := GLMTabulator new.
	browser column: #one; column: #two.
	browser transmit to: #one; andShow: [:a | a list display: [:x | 1 to: x ] ].
	browser transmit from: #one; to: #two; andShow: [:a | a list display: [:x | 1 to: x ] ].
	browser transmit from: #two port: #toSelect; toOutsidePort: #toSelect.
	browser transmit fromOutsidePort: #toSelect; to: #one port: #selection.
	browser transmit fromOutsidePort: #toSelect; to: #two port: #selection.
	window := browser openOn: 100.
	((browser paneNamed: #one) port: #selection) value: 10.
	((browser paneNamed: #two) port: #toSelect) value: 5.
	self assert: ((browser paneNamed: #one) port: #selection) value equals: 5.
	self assert: ((browser paneNamed: #two) port: #selection) value equals: 5.

	World doOneCycle.

	morphOne := self findWherePresentationIs: (browser paneNamed: #one) presentations first in: window.
	self assert: morphOne notNil.
	self assert: morphOne selectedMorph notNil.
	
	morphTwo := self findWherePresentationIs: (browser paneNamed: #two) presentations first in: window.
	self assert: morphTwo notNil.
	self assert: morphTwo selectedMorph notNil
]
