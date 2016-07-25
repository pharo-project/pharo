tests
testCriticsOf

	| rule |
	rule := RBCompositeLintRule allGoodRules.
	checker 
		rule: rule;
		environment: self package asEnvironment;
		run. 
	
	self assert: (checker criticsOf: RBOnlyReadOrWrittenTemporaryRule new) size  = 3.
	self assert: (( checker criticsOf: RBOnlyReadOrWrittenTemporaryRule new ) anySatisfy: [:each| each = (MFClassB>>#method3)]).
	self assert: (( checker criticsOf: RBOnlyReadOrWrittenTemporaryRule new) anySatisfy: [:each| each = (MFClassA>>#method)]).