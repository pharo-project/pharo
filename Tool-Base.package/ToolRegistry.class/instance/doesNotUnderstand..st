handling DNU
doesNotUnderstand: aMessage
	"Return a tool identified by a message selector.
	We need to handle special case, when no debugger registered.
	"
	| tool |
	tool := tools at: aMessage selector ifAbsent: nil.
	tool ifNotNil: [ ^ tool value ].

	^ (aMessage selector == #debugger) 
		ifTrue: [ 
			Smalltalk 
				logError: 'No debugger installed to handle debug request.' 
				inContext: thisContext.
		
			nil. "Smalltalk snapshot: false andQuit: true."
		]
		ifFalse: [ super doesNotUnderstand: aMessage ]
