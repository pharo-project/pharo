stack parameter classification
stackParameterClass
	^ self isPointer ifTrue: [ #integer ] ifFalse: [ self stackValueParameterClass ]