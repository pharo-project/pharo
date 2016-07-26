private
simulatePrimitive: primitiveIndex in: aMethod receiver: aReceiver arguments: arguments
	| key simulator |
	
	key := primitiveIndex = 117 
		ifTrue: [ | literal |
			literal := aMethod literalAt: 1.
			"primitive name, module name"
			{literal second. literal first}] 
		ifFalse: [ primitiveIndex ].
	
	simulator := self class specialPrimitiveSimulators at: key ifAbsent: [
		 "named primitives"
		^  primitiveIndex = 117
				ifTrue: [
					self withoutPrimitiveTryNamedPrimitiveIn: aMethod for: aReceiver withArgs: arguments.
					"this uses primitive 218, which doesn't works as expected... 
					self tryNamedPrimitiveIn: method for: receiver withArgs: arguments " ]
				ifFalse: [ aReceiver tryPrimitive: primitiveIndex withArgs: arguments ]]. 
	
	^ simulator 
		simulatePrimitiveFor: aMethod
		receiver: aReceiver
		arguments: arguments
		context: self