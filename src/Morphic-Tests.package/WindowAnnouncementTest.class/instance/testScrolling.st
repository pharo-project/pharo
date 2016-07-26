tests - window change
testScrolling
	"This test tests the scrolling values of a scrollpane. We create a reeeeeally big (enormous) morph, to be sure its size is bigger than the screen and therefore its enclosing window has scrollbars.
	When screen sizes become bigger, you should increase the value, or fix morphic, what happens first :).
	"
	| myMorph pane t |
	window := SystemWindow labelled: 'foo'.
	myMorph := Morph new.
	myMorph extent: 10000000000@1000000000.
	pane := ScrollPane new.
	pane scroller addMorph: myMorph.
	window
		addMorph: pane
		fullFrame: LayoutFrame identity.
	t := 0 @ 0.
	window openInWorld.
	window announcer when: WindowScrolling do: [:ann | t := t + ann step ].
	pane hScrollBarValue: 10.
	pane vScrollBarValue: 5.

	window delete.
	
	self assert: (t = (10 @ 5)).
	window := nil