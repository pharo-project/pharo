tests
testWithDisplay
	| browser dynamic stacker text textMorph |
	browser := GLMTabulator new column: #stacker; yourself.
	browser transmit to: #stacker; andShow: [:c | 
		c dynamic
			display: [:number | |st|
				st := GLMStacker new.
				st aPane: #all.
				st transmit to: #all; andShow: [:a | a text display: ['All ', number asString]].
				st]].
	window := browser openOn: 10.
	dynamic := browser panes first presentations first.
	stacker := dynamic cachedPresentation first.
	self assert: stacker panes first presentations size equals: 1.
	text := stacker panes first presentations first.
	textMorph := self findWherePresentationIs: text in: window.
	self assert: textMorph text asString equals: 'All 10'
	