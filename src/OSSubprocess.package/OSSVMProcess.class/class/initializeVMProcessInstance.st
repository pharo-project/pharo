initialize - release
initializeVMProcessInstance
	VMProcess isNil 
		ifTrue: [ VMProcess := self basicNew. ]
		ifFalse: [ VMProcess finalizePreviousSession ].
	VMProcess initialize.