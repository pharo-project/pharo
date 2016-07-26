tests
testEmbeddedBrowserRefreshesRendering
	| tabulator finder tabulator1 textMorph1 textMorph2 content1 details1 |
	"self debug: #testEmbeddedBrowserRefreshesRendering"
	tabulator := GLMTabulator new.
	tabulator
		row: #content;
		row: #details.
	tabulator transmit
		to: #content;
		andShow: [ :a | a list display: [ :x | 1 to: x ] ].
	tabulator transmit
		from: #content;
		to: #details;
		andShow: [ :a | a text ].
	tabulator transmit
		from: #content;
		toOutsidePort: #selection.
	finder := GLMFinder new.
	finder show: [ :a | a custom: tabulator ].
	window := finder openOn: 42.
	tabulator1 := finder panes first presentations first.
	content1 := tabulator1 panes first.
	details1 := tabulator1 panes second.
	(content1 port: #selection) value: 41.
	textMorph1 := self findWherePresentationIs: details1 presentations first in: window.
	self assert: textMorph1 model getText = '41'.
	(content1 port: #selection) value: 42.
	self assert: tabulator1 == finder panes first presentations first.
	self assert: details1 == tabulator1 panes second.
	self assert: details1 presentations first text asString equals: '42'.
	textMorph2 := self findWherePresentationIs: details1 presentations first in: window.
	self assert: textMorph2 model getText asString equals: '42'