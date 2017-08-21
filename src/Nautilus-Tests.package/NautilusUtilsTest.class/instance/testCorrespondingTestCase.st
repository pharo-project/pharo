tests
testCorrespondingTestCase
	"tests that classes and their meta classes have the same test class
	corresponding to them"

	{(Behavior -> BehaviorTest).
	(Behavior class -> BehaviorTest).
	(BehaviorTest -> nil).
	(Class -> ClassTest).
	(Class class -> ClassTest).
	(ClassTest -> nil).
	(NautilusUtilsTestStub -> NautilusUtilsTestStubTest).
	(NautilusUtilsTestStub class -> NautilusUtilsTestStubTest).
	(NautilusUtilsTestStubTest -> nil).
	(NautilusUtilsTestStubUntested -> nil).
	(NautilusUtilsTestStubUntested class -> nil)}
		do: [ :each | self assert: each key correspondingTestCase == each value ]