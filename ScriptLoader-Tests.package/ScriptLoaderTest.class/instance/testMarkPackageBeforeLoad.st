testing
testMarkPackageBeforeLoad
	"self debug: #testMarkPackageBeforeLoad"
	

	| s |
	s := ScriptLoader new.
	s resetPackagesBeforeLastLoad.
	self assert: s diffPackages size = s currentVersionsToBeSaved size.
	self assert: s  = s.
	
	s markPackagesBeforeNewCodeIsLoaded.
	self assert: s diffPackages isEmpty