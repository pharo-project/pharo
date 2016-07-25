accessing
helpBrowserWindow
	"Yes this is a hack"
	
	^ World systemWindows 
		detect: [ :each | each model class = HelpBrowser ] 
		ifNone: [ nil ]