private
testRunner
	(self hasOption: 'junit-xml-output') ifTrue: [ ^ HDTestReport ].
	
	self class environment at: #CommandLineTestRunner ifPresent: [ :commandLineTestRunner |
		(self hasOption: 'no-xterm') ifTrue: [ ^ commandLineTestRunner ].
		^ self class environment at: #VTermTestRunner
	] ifAbsent: [ self error: 'no tests output available, try to use the option --junit-xml-output' ]
