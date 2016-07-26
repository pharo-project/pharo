testing
testIsClassOrTrait

	self 
		assert: TBehavior isClassOrTrait;
		assert: Class isClassOrTrait;
		assert: Class class isClassOrTrait;
		deny: Object new isClassOrTrait;
		assert: TBehavior classTrait isClassOrTrait.