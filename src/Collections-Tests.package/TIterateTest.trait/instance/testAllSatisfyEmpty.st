tests - iterating
testAllSatisfyEmpty

	self assert: ( self empty allSatisfy: [:each | false]).
	