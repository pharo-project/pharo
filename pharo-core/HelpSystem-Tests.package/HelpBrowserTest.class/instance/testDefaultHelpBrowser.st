testing
testDefaultHelpBrowser
	
	| current replacement instance |
	current := self defaultTestClass defaultHelpBrowser.
	replacement := AdvancedHelpBrowserDummy.
	[
	  self defaultTestClass defaultHelpBrowser: replacement.
	  self assert: self defaultTestClass defaultHelpBrowser == replacement.
 
	  instance := self defaultTestClass open.
	  self assert: instance rootTopic notNil.
	  self assert: instance isOpen.
	] ensure: [ self defaultTestClass defaultHelpBrowser: current ]
	 