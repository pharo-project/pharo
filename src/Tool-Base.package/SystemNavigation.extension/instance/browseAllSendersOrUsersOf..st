*Tool-Base
browseAllSendersOrUsersOf: aLiteralOrClass 
	"Create and schedule a message browser on each method that refers to 
	a literal or class name"
   | senders classRefs |
   senders := self allCallsOn: aLiteralOrClass.
   classRefs := self classRefsToClassName: aLiteralOrClass.
	^ self openBrowserFor: aLiteralOrClass withMethods: (senders, classRefs)