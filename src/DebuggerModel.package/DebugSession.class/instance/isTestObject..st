testing
isTestObject: anObject

	"I'm not sure this is the best way to doit because it creates a coupling with TestCase, but due that SUnit is part of the core I think it is not bad after all - Hernan'"
	^ anObject isKindOf: TestCase