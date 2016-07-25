testing-environments
testNotEnvironment
	| notPrintStringEnvironment printStringEnvironment kernelEnvironment notKernelEnvironment|
	printStringEnvironment := universalEnvironment referencesTo: #printString.
	notPrintStringEnvironment := printStringEnvironment not.
	kernelEnvironment := (RBPackageEnvironment packageName: 'Kernel').
	notKernelEnvironment := kernelEnvironment not.

	self universalTestFor: notPrintStringEnvironment.
	self assert: (notPrintStringEnvironment referencesTo: #printString) isEmpty.
	self assert: (notPrintStringEnvironment not includesClass: RBBrowserEnvironmentTest).
	self assert: (notPrintStringEnvironment not 
			includesSelector: #testNotEnvironment
			in: RBBrowserEnvironmentTest).
	self assert: (notKernelEnvironment includesPackage: 'Kernel' asPackage) not.
	self assert: (notKernelEnvironment & kernelEnvironment) packages isEmpty