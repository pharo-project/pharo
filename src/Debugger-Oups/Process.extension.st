Extension { #name : 'Process' }

{ #category : '*Debugger-Oups' }
Process >> debugWithTitle: title [

	| context |
	context := self isActiveProcess
		           ifTrue: [ thisContext ]
		           ifFalse: [ self suspendedContext ].
	UIManager default
		requestDebuggerOpeningForProcess: self
		named: title
		inContext: context
]
