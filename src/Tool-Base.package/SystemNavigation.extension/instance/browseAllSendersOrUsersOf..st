*Tool-Base
browseAllSendersOrUsersOf: aLiteralOrClass 
	"Create and schedule a message browser on each method that refers to 
	a literal or class name"
   | senders globalRefs |
   senders := self allCallsOn: aLiteralOrClass.
   globalRefs := self allGlobalRefsOn: aLiteralOrClass.
	^ self openBrowserFor: aLiteralOrClass withMethods: (senders, globalRefs)