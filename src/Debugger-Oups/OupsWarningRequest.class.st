"
Warnings are requested as debug requests, but should not open debugger directly.
Warning requests are submitted through a different _debugging channel_.
"
Class {
	#name : #OupsWarningRequest,
	#superclass : #OupsDebugRequest,
	#category : #'Debugger-Oups-infrastructure'
}

{ #category : #submit }
OupsWarningRequest >> submit [
	OupsDebuggerSystem new handleWarningDebugRequest: self
]
