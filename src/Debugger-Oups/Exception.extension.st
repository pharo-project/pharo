Extension { #name : 'Exception' }

{ #category : '*Debugger-Oups' }
Exception >> debug [

	"requests a debugger on myself"

	<debuggerCompleteToSender>
	UIManager default requestDebuggerOpeningFor: self
]
