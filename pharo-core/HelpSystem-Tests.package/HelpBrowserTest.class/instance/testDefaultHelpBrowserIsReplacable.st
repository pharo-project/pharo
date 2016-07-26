testing
testDefaultHelpBrowserIsReplacable
	
	| current replacement instance |
	"save the one that is registered"
	current := self defaultTestClass defaultHelpBrowser.
	replacement := AdvancedHelpBrowserDummy.
	[
	  self defaultTestClass defaultHelpBrowser: replacement.
	  self assert: self defaultTestClass defaultHelpBrowser == replacement.	  
	  instance := self defaultTestClass open.
	  
	] ensure: [
		self defaultTestClass defaultHelpBrowser: current
	]
	 