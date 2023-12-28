Extension { #name : 'Exception' }

{ #category : '*Debugger-Oups' }
Exception >> debug [
	"requests a debugger on myself"

	<debuggerCompleteToSender>
	(OupsDebugRequest newForContext: self stack)
		label: self message;
		submit
]
