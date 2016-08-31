tests
testCorrespondingTestCase
	"tests that classes and their meta classes have the same test class
	corresponding to them"

	self
		assert: NautilusUtilsTestStub correspondingTestCase == NautilusUtilsTestStubTest;
		assert: NautilusUtilsTestStub class correspondingTestCase == NautilusUtilsTestStubTest;
		assert: NautilusUtilsTestStubTest correspondingTestCase isNil;
		assert: Behavior correspondingTestCase == BehaviorTest;
		assert: Behavior class correspondingTestCase == BehaviorTest;
		assert: BehaviorTest correspondingTestCase isNil;
		assert: Class correspondingTestCase == ClassTest;
		assert: Class class correspondingTestCase == ClassTest;
		assert: ClassTest correspondingTestCase isNil;
		assert: Metaclass correspondingTestCase == MetaclassTest;
		assert: Metaclass class correspondingTestCase == MetaclassTest;
		assert: MetaclassTest correspondingTestCase isNil.

	self assert: NautilusUtilsTestStubUntested correspondingTestCase isNil.