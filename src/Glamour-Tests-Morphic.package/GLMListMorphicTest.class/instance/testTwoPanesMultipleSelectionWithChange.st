tests
testTwoPanesMultipleSelectionWithChange
	| browser treeMorphOne treeNodeMorph treeMorphTwo |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two;
		column: #three.
	(browser transmit)
		to: #one;
		andShow: [ :a | a list display: [ :x | 1 to: x ] ].
	(browser transmit)
		to: #two;
		from: #one;
		andShow: [ :a | 
					(a list)
						beMultiple;
						display: [ :x | x * 10 to: x * 10 + 4 ] ].
	(browser transmit)
		to: #three;
		from: #two;
		andShow: [ :a | a text ].
	window := browser openOn: 4.	"we select 2 in the first pane"
	treeMorphOne := (window submorphs last submorphs at: 3) submorphs first submorphs first.
	treeNodeMorph := treeMorphOne scroller submorphs second.
	self send: treeMorphOne mouseUpOnItem: treeNodeMorph.	"we select all in the second pane"
	treeMorphTwo := (window submorphs last submorphs at: 4) submorphs first submorphs first.
	treeNodeMorph := treeMorphTwo scroller submorphs first.
	self send: treeMorphTwo mouseUpOnItem: treeNodeMorph.
	treeNodeMorph := treeMorphTwo scroller submorphs second.
	self send: treeMorphTwo mouseUpWithShift: true onItem: treeNodeMorph.
	self assert: ((browser paneNamed: #two) port: #selection) value asOrderedCollection equals: #(20 21) asOrderedCollection.	"we select 4 in the first pane"
	treeMorphOne := (window submorphs last submorphs at: 3) submorphs first submorphs first.
	treeNodeMorph := treeMorphOne scroller submorphs last.
	self send: treeMorphOne mouseUpOnItem: treeNodeMorph.	"we select the first two in the second pane"
	treeMorphTwo := (window submorphs last submorphs at: 4) submorphs first submorphs first.
	self assert: (treeMorphTwo model roots collect: #item) equals: #(40 41 42 43 44)  asOrderedCollection.
	treeNodeMorph := treeMorphTwo scroller submorphs first.
	self send: treeMorphTwo mouseUpOnItem: treeNodeMorph.
	treeNodeMorph := treeMorphTwo scroller submorphs second.
	self send: treeMorphTwo mouseUpWithShift: true onItem: treeNodeMorph.
	self assert: ((browser paneNamed: #two) port: #selection) value asOrderedCollection equals: #(40 41)  asOrderedCollection