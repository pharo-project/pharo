tests
testDefault
	"When opened, there are plenty of packages, ie., working copies that are listed"
	self assert: b workingCopies notNil.
	self assert: b workingCopies notEmpty.
	
	"When opening, there is no repository selected"
	self assert: b repository isNil.
	
	"No repository is selected"
	self assert: b repositorySelection equals: 0