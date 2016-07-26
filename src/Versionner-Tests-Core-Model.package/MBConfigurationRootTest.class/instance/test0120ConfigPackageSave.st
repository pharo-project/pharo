tests
test0120ConfigPackageSave
	"Issue 125 test
	 	- load configuration dkh.3
		- load version 1.2
		- modify configuration and validate
		- save configuration package and validate"
	| configClass configClassName configInfo expected infos suggestedName signature |
	"load configuration dkh.3"
	gofer version: 'ConfigurationOfMBFooTests-dkh.3'.
	gofer load.
	configClassName := #ConfigurationOfMBFooTests.
	self assert: (configClass := Smalltalk at: configClassName ifAbsent: []) notNil.
	configInfo := configurationRoot configurationInfoFor: configClass.

	"load version 1.2"
	((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.2')  load.
	self assert: configInfo workingCopy notNil. 
	
	self assert: configInfo printString = 'MBFooTests 1.2(dkh.3)'.
	self assert: configInfo currentVersion versionString = '1.2'.
	self assert: configInfo printString = 'MBFooTests 1.2(dkh.3)'.
	self assert: configInfo asStringOrText isString.
	
	"modify class in ConfigurationOfMBFooTests class"
	(Smalltalk at: #ConfigurationOfMBFooTests)
		compile: 'bar ^self' classified: 'mod'.

	self assert: configInfo printString = '* MBFooTests 1.2(dkh.3)'.
	self assert: configInfo asStringOrText isText.

	"save the configuraiton package"
	[ Gofer new
		disablePackageCache;
		repository: (MCDictionaryRepository new);
		package: configClassName asString;
		commit ] 
			on: MCVersionNameAndMessageRequest
			do: [:ex |  
				suggestedName := ex suggestedName.
				ex resume: { suggestedName.  'commit comment'}].

	signature := suggestedName copyAfterLast: $-.
	self assert: configInfo printString = ('MBFooTests 1.2(', signature, ')').
	self assert: configInfo asStringOrText isString.
