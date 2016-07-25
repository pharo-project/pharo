test addition
testRemoveShouldRaiseErrorWhenMethodIsNotInPackage
	"self debug: #testRemoveShouldRaiseErrorWhenMethodIsNotInPackage"
	
	self should: [p2 removeMethod: a2>>#methodDefinedZork] raise: Error