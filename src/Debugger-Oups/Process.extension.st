Extension { #name : 'Process' }

{ #category : '*Debugger-Oups' }
Process >> debug [
	^ self debugWithTitle: 'Debug'
]

{ #category : '*Debugger-Oups' }
Process >> debugWithTitle: title [

	<debuggerCompleteToSender>
	| context |
	context := self isActive
		           ifTrue: [ thisContext ]
		           ifFalse: [ self suspendedContext ].

	Smalltalk logError: context printString inContext: context.

	(OupsDebugRequest newForContext: context)
		process: self;
		label: title;
		submit
]
