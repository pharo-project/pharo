initialize
quickMethod
	instructionStream method isReturnSelf
		ifTrue: [ ^ self methodReturnReceiver ].
	instructionStream method isReturnSpecial
		ifTrue: [ 
			^ self
				methodReturnConstant:
					(instructionStream method encoderClass specialLiterals at: instructionStream method primitive - 256) ].
	instructionStream method isReturnField
		ifTrue: [ 
			self pushReceiverVariable: instructionStream method returnField.
			^ self methodReturnTop ].
	self error: 'quick method inconsistency'