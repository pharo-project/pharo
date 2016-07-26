error handling
explicitRequirement
	"If one of the superclasses can perform the selector, we execute the method of that class, otherwise, the explicit requirement error is thrown"

	| originalMethod originalArguments errorBlock originalReceiver callingContext originalSelector |
	errorBlock := [ ^ self error: 'Explicitly required method' ].
	callingContext := thisContext sender.
	originalMethod := callingContext method.
	originalMethod isFromTrait
		ifFalse: errorBlock.
		
	originalReceiver := callingContext receiver.
	originalSelector := originalMethod selector.
	originalArguments := callingContext arguments.
	
	originalReceiver class superclass
		withAllSuperclassesDo: [ :superClass | 
			superClass
				compiledMethodAt: originalSelector
				ifPresent: [ :method | 
					(method isProvided or: [ method isFromTrait not ])
						ifTrue: [ 
							callingContext return: (method valueWithReceiver: originalReceiver arguments: originalArguments) ] ]
				ifAbsent: [  ] ].

	errorBlock value