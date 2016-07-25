*Tool-Base
browseAllCallsOn: aLiteral localTo: aClass
	"Create and schedule a message browser on each method in or below the given class that refers to
	aLiteral. For example, SystemNavigation new browseAllCallsOn: #open:label: localTo: CodeHolder."


	^ self browseAllSendersOf: aLiteral localTo: aClass