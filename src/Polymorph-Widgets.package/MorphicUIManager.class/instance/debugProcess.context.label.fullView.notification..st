debug
debugProcess: process context: context label: title fullView: bool notification: notificationString
	"Open a notifier in response to an error, halt, or notify. A notifier view
	just shows a short view of the sender stack and provides a menu that
	lets the user open a full debugger."

	| debugSession |

	debugSession := process newDebugSessionNamed: title startedAt: context.
	
	debugSession logStackToFileIfNeeded.
	debugSession isAboutUIProcess ifTrue: [ 
		DefaultExecutionEnvironment beActiveDuring: [self spawnNewProcess]].
		
	self defer:	[ 
			"schedule debugger in deferred UI message to address
			redraw problems after opening a debugger e.g. from
			the testrunner."
		[ Smalltalk tools debugger 
				openOn: debugSession withFullView: bool andNotification: notificationString.
		] on: Error do: [ :ex | 	debugSession signalDebuggerError: ex] 
	].

	process suspend.
