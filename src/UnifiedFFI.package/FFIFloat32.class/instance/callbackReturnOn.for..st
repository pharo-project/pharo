accessing
callbackReturnOn: callbackContext for: aFloat
	^ FFIArchitecture forCurrentArchitecture returnSingleFloatsAsDoubles
		ifTrue: [ callbackContext floatResult: aFloat ]
		ifFalse: [ callbackContext singleFloatResult: aFloat ]