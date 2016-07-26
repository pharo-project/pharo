tests
testPresentationBoundPort
	| browser outerPort1 outerPort2 transmission1 transmission2 |
	browser := GLMExplicitBrowser new.
	browser addNewPane.
	GLMTestPane new addPresentation: browser.
	outerPort1 := (GLMPresentationBoundPort new)
		presentation: browser;
		name: #outerPort1;
		yourself.
	outerPort2 := (GLMPresentationBoundPort new)
		presentation: browser;
		name: #outerPort2;
		yourself.	"Capturing"
	browser
		addTransmission:
			(transmission1 := (GLMMockSimpleTransmission new)
				addActiveOrigin: outerPort1;
				destination: (browser panes first port: #innerPort1);
				yourself).	"Publishing"
	browser
		addTransmission:
			((transmission2 := GLMMockSimpleTransmission new)
				addActiveOrigin: (browser panes first port: #innerPort2);
				destination: outerPort2;
				yourself).
	(browser pane port: #outerPort1) value: 42.
	self assert: (browser panes first port: #innerPort1) value equals: 42.
	self assert: transmission1 wasTransmited.
	self deny: transmission2 wasTransmited.	"self assert: (browser pane port: #outerPort1) seenTransmissions size = 0. "	"self 
		assert: 
			((browser pane port: #outerPort1) seenTransmissions first isKindOf: 
					GLMSimpleTransmission). 
	self 
		assert: 
			((browser pane port: #outerPort1) seenTransmissions first origin isKindOf: 
					GLMSimplePort). 
	self 
		assert: 
			(browser pane port: #outerPort1) seenTransmissions first destination 
				== (browser pane port: #outerPort1). "	"self 
		assert: 
			(browser panes first port: #innerPort1) seenTransmissions size = 1. 
	self 
		assert: 
			(browser panes first port: #innerPort1) seenTransmissions first 
				== browser transmissions first. "
	(browser panes first port: #innerPort2) value: 43.
	self assert: (browser pane port: #outerPort2) value equals: 43	"self 
		assert: 
			(browser panes first port: #innerPort2) seenTransmissions size = 0. "	"self 
		assert: 
			((browser panes first port: #innerPort2) seenTransmissions first isKindOf: 
					GLMSimpleTransmission). 
	self 
		assert: 
			((browser panes first port: #innerPort2) seenTransmissions first origin 
				isKindOf: GLMSimplePort). 
	self 
		assert: 
			(browser panes first port: #innerPort2) seenTransmissions first destination 
				== (browser panes first port: #innerPort2). "	"self assert: (browser pane port: #outerPort2) seenTransmissions size = 1. 
	self 
		assert: 
			(browser pane port: #outerPort2) seenTransmissions first 
				== browser transmissions last"