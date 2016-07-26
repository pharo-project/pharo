tests
testExternalAtPutFloat
	| random array |
	random := Random new.
	[
		array := FFIExternalArray externalNewType: 'float' size: 10.
		1 to: 10 do: [ :index | array at: index put: random next ].
		1 to: 10 do: [ :index | self assert: (array at: index) isFloat ] ]
	ensure: [ array free ].