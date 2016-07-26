tests
testValidSubscriptionsInComplexBrowser
	| browser navigator tabulator1 tabulator2 pane1 pane2 content1 content2 details1 details2|
	
	navigator := GLMTabulator new.
	navigator row: #content; row: #details.
	navigator transmit to: #content; andShow: [:a | a list display: #allSuperclasses].
	navigator transmit from: #content; to: #details; andShow: [ :a | a text].
	navigator transmit from: #content; toOutsidePort: #selection.
	 
	browser := GLMFinder new. 
	browser show: [:a | a custom: navigator].
	self assert: browser panes isEmpty.
	(browser pane port: #entity) value: String. 
	self assert: browser panes size equals: 1. 
	pane1 := browser panes first.
	tabulator1 := pane1 presentations first.
	content1 := tabulator1 panes first.
	self assert: content1 presentations first displayValue size equals: 5. "5 superclasses of String".
	(content1 port: #selection) value: Collection. "Assumes Collection is a superclass of String" 
	self assert: browser panes size equals: 2. 
	pane2 := browser panes second.
	tabulator2 := pane2 presentations first.
	content2 := tabulator2 panes first.
	self assert: content1 presentations first displayValue size equals: 5. "5 superclasses of String".
	self assert: content2 presentations first displayValue size equals: 2. "2 superclasses of Collection".
	details1 := tabulator1 panes second.
	self assert: details1 presentations first displayValue equals: Collection.
	(content2 port: #selection) value: Object. "Assumes Object is a superclass of Collection"
	self assert: details1 presentations first displayValue equals: Collection.
	details2 := tabulator2 panes second.
	self assert: details2 presentations first displayValue equals: Object.
	self assert: content1 presentations first displayValue size equals: 5. "5 superclasses of String".
	self assert: content2 presentations first displayValue size equals: 2. "2 superclasses of Collection".