as yet unclassified
argumentsFor: stackPointer context: callbackContext
	| index architecture intRegisterCount intRegisterSize intRegisterIndex floatRegisterCount floatRegisterSize floatRegisterIndex floatRegisters intRegisters structureRegisterLayout |
	self flag: 'TODO: Refactor and improve this code.'.
	architecture := FFIArchitecture forCurrentArchitecture.
	
	intRegisterCount := architecture integerRegisterCountForParameterPassing.
	intRegisterSize := architecture integerRegisterSize.
	intRegisterIndex := 0.
	
	floatRegisterCount := architecture floatRegisterCountForParameterPassing.
	floatRegisterSize := architecture floatRegisterSize.
	floatRegisterIndex := 0.
	
	floatRegisters := callbackContext floatregargsp.
	intRegisters := callbackContext intregargsp.
	
	index := 1 + architecture shadowCallSpaceSize.
	^ functionSpec arguments 
		collect: [ :each | | value parameterClass |
			parameterClass := each stackParameterClass.
			(parameterClass == #integer and: [ intRegisterIndex < intRegisterCount and: [each typeSize <= intRegisterSize ]]) ifTrue: [
				value := each callbackValueFor: intRegisters at: intRegisterIndex*intRegisterSize + 1.
				intRegisterIndex := intRegisterIndex + 1
			] ifFalse: [ 
				(parameterClass == #float and: [ floatRegisterIndex < floatRegisterCount and: [each typeSize <= floatRegisterSize ]]) ifTrue: [
					value := each callbackValueFor: floatRegisters at: floatRegisterIndex*floatRegisterSize + 1.
					floatRegisterIndex := floatRegisterIndex + 1
				] ifFalse: [
					(parameterClass == #structure and:
					[ (structureRegisterLayout := architecture computeStructureRegisterPassingLayout: each objectClass) isNotNil and:
					[intRegisterIndex + structureRegisterLayout integerRegisterCount <= intRegisterSize and:
					[floatRegisterIndex + structureRegisterLayout floatRegisterCount <= floatRegisterSize]]]) ifTrue: [
						"This is structure whose content was splitted in registers."
						value := each objectClass new.
						structureRegisterLayout fields do: [ :structureField |
							structureField registerClass == #integer ifTrue: [
								1 to: structureField size do: [ :i |
									value getHandle byteAt: structureField offset + i put:
										(intRegisters unsignedByteAt: intRegisterIndex*intRegisterSize + i)
								].
							] ifFalse: [
								self assert: structureField registerClass == #float.
								1 to: structureField size do: [ :i |
									value getHandle byteAt: structureField offset + i put:
										(floatRegisters unsignedByteAt: floatRegisterIndex*floatRegisterSize + i)
								].
								floatRegisterIndex := floatRegisterIndex + 1
							] 
						]
					] ifFalse: [ 
						"Memory parameter" 
						value := each callbackValueFor: stackPointer at: index.
						index := index + each typeSize.
					]
				].
			].
		
			value ].	