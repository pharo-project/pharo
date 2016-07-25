tests
testInterdependentPanesShouldUpdateTheFocusTriggerPane
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
