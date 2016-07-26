testing
isInterruptedContextATest

	^ (self isTestObject: self interruptedContext receiver) and: [ self isTestMethod: self interruptedContext method of: self interruptedContext receiver ]