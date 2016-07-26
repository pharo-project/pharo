tests
testOnePanesShouldUpdateTheRenderedSelectionOnTheTriggerPane

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