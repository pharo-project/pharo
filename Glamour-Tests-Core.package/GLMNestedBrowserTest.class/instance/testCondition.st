tests
testCondition
	"This tests that the nested browser is controlled by the condition"

	| browser1 browser2 transmission1 |
	browser1 := GLMExplicitBrowser new.
	browser2 := GLMExplicitBrowser new.
	browser1 title: 'browser1'.
	browser2 title: 'browser2'.
	browser2
		transformation: [ :x | 
					x = 42
						ifFalse: [ 
							self fail.
							42 ]
						ifTrue: [ 84 ] ];
		condition: [ :x | x = 42 ].
	browser1 addNewPane name: 'pane1'.
	transmission1 := GLMTransmission new ensureReplacePresentationsStrategy.
	transmission1 transmissionStrategy addPresentation: browser2.
	browser1
		addTransmission:
			(transmission1
				addActiveOrigin:
						((GLMPresentationBoundPort new)
								presentation: browser1;
								name: #entity;
								yourself);
				destination: (browser1 panes first port: #entity);
				yourself).
	self assert: (browser1 pane port: #entity) value isNil.
	self assert: (browser1 panes first port: #entity) value isNil.
	self assert: browser1 panes first presentations isEmpty.
	(browser1 pane port: #entity) value: 41.
	self assert: (browser1 pane port: #entity) value equals: 41.
	self assert: (browser1 panes first port: #entity) value equals: 41.
	self assert: browser1 panes first presentations size equals: 1.
	self assert: browser1 panes first matchingPresentations size equals: 0.
	(browser1 pane port: #entity) value: 42.
	self assert: (browser1 pane port: #entity) value equals: 42.	"	self assert: (browser1 panes first port: #entity) presentations value = 84.
"
	self assert: browser1 panes first presentations size equals: 1.
	self assert: browser1 panes first matchingPresentations size equals: 1