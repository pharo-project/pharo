tests
testWithPresentation
	| browser dynamic text textMorph |
	browser := GLMTabulator new column: #stacker; yourself.
	browser transmit to: #stacker; andShow: [:c | 
		c dynamic
			presentation: [:presentation :number |
					presentation text display: ['All ', number asString]]].
	window := browser openOn: 10.
	dynamic := browser panes first presentations first.
	text := dynamic cachedPresentation first.
	textMorph := self findWherePresentationIs: text in: window.
	self assert: textMorph text asString equals: 'All 10'