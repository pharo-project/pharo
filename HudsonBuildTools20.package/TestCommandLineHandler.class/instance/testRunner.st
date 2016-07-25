private
testRunner
	(self hasOption: 'junit-xml-output') ifTrue: [ ^ HDTestReport ].
	(self hasOption: 'no-xterm') ifTrue: [ ^ CommandLineTestRunner ].
	^ VTermTestRunner