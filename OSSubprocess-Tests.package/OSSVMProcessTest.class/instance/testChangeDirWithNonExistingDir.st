tests
testChangeDirWithNonExistingDir
	| oldDir | 
	oldDir := self systemAccessor getcwd.
	[ OSSVMProcess vmProcess lockCwdWithValue: '/tmpWhatever'  during: [ ] ]
	on: Error do:  [ :ex | ex printString includesSubstring: 'does not exist' ].
	self assert: self systemAccessor getcwd equals: oldDir
				