tests
testStatusbar
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