tests
testTwoInterdependentPanesShouldUpdateTheRenderedSelectionOnTheTriggerPane

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