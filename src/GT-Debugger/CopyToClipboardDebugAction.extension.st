Extension { #name : #CopyToClipboardDebugAction }

{ #category : #'*GT-Debugger-Actions' }
CopyToClipboardDebugAction class >> gtActionFor: aDebugger [
	<gtStackDebuggingAction>
	
	^ (self forDebugger: aDebugger)
			order: 35
]
