actions
postload: loader package: packageSpec

	| gofer |
	
	Nautilus registerToolsOn: Smalltalk tools.
	Smalltalk tools register: TestRunner as: #testRunner.
	Smalltalk tools register: MCWorkingCopyBrowser as: #monticelloBrowser.
	
	BIConfigurableFormatter initialize.
	
	Smalltalk garbageCollect.
	Smalltalk cleanOutUndeclared. 

	Author reset.
