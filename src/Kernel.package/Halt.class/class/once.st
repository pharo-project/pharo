halting
once
	"Stop once and only once the execution of the method containing the expression Halt once. If you need to get it stops another time after the first stop, use Halt resetOnce."

	| callingContext senderContext node |
	<debuggerCompleteToSender>
		
	"if the compiler is not loaded, we fall back on the standard halt "
	self environment at: #OpalCompiler ifAbsent: [ ^self now ].	
	
	callingContext := (thisContext findContextSuchThat: [ :ctx | 
		ctx method sendsAnySelectorOf: #(haltOnce) ]
	) ifNil: [ thisContext sender ].
	
	senderContext := thisContext.
	
	LocalRecursionStopper during: [
		"we need to ignore when the code below calls us again"
		node := callingContext sourceNodeExecuted.
		(node hasProperty: #Once) ifTrue: [ ^ self ]. 
		node propertyAt: #Once put: true.
		
		self signalIn: senderContext
		
	].
	
	
