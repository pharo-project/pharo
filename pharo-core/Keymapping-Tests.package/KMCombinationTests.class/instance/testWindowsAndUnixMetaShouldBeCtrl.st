tests
testWindowsAndUnixMetaShouldBeCtrl
	OSPlatform current isMacOS ifTrue: [ ^ self skip ].
	self assert: $n meta equals: $n ctrl.
	self assert: $n ctrl equals: $n meta.
	self assert: $n meta shift equals: $n ctrl shift.
	self assert: $n ctrl shift equals: $n meta shift