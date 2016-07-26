*Tool-Base
browseAllReferencesTo: aLiteral 
	"Create and schedule a message browser on each method that refers to 
	aLiteral. For example, SystemNavigation new browseAllSendersOf: #printOn:."

	^ self openBrowserFor: aLiteral withMethods: (self allCallsOn: aLiteral) 