tests
testExternalAt
	| externalArray |
	
	[
	 	externalArray := FFIExternalArray externalNewType: 'int' size: 10.
		self assert: externalArray size equals: 10.
		"External is not initialized, so I cannot check for contents unless I assign them"
		1 to: 10 do: [ :index | externalArray at: index put: 0 ].
		"Testing accessing"
		1 to: 10 do: [ :index | self assert: (externalArray at: index) equals: 0 ].
	] ensure: [ 
		externalArray free ] 
