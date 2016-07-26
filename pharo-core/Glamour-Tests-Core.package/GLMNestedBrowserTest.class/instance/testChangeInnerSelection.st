tests
testChangeInnerSelection
	"Tests setting the selection of an inner pane from the oustide. 
 
	The test sets up a browser with two panes. The first contains a 
	list of numbers, the second a browser which again contains one 
	pane with a list of the same numbers. When the selection in the 
	first pane changes, we want the same number to be selected in 
	the second pane. 
 
	To achieve this, we pull in a port on the second pane and copy 
	it to the inner pane using a simple transmission."

	| browser1 browser2 list1 list2 |
	browser1 := GLMExplicitBrowser new.
	browser2 := GLMExplicitBrowser new.
	list1 := GLMListPresentation new.
	list2 := GLMListPresentation new.
	browser1 addNewPane.
	browser1 addNewPane.
	browser2 addNewPane.
	browser1 panes first addPresentation: list1.
	browser1 panes last addPresentation: browser2.
	browser2 panes first addPresentation: list2.
	browser1
		addTransmission:
			((GLMTransmission new)
				addActiveOrigin: (browser1 panes first port: #selection);
				destination: (browser1 panes last port: #innerSelection);
				yourself).
	browser2
		addTransmission:
			((GLMTransmission new)
				addActiveOrigin:
						((GLMPresentationBoundPort new)
								presentation: browser2;
								name: #innerSelection;
								yourself);
				destination: (browser2 panes first port: #selection);
				yourself).
	list1 entity: #(1 2 3).
	list2 entity: #(1 2 3).
	list1 selection: 1.
	self assert: list2 selection equals: 1.
	list1 selection: 2.
	self assert: list2 selection equals: 2