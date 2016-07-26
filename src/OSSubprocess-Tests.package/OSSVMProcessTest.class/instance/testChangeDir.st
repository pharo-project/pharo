tests
testChangeDir
	| oldDir | 
	oldDir := self systemAccessor getcwd.
	OSSVMProcess vmProcess
		lockCwdWithValue: '/tmp' 
		during: [
			"Grrr in latest OSX /tmp is mapped to /private/tmp..."
			self assert: ((self systemAccessor getcwd = '/tmp') or: [ self systemAccessor getcwd = '/private/tmp' ]).
		].
	self assert: self systemAccessor getcwd equals: oldDir
				