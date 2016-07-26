tests
testCorrespondingTestClass
	self assert: (Behavior correspondingForTest == BehaviorTest).
	self assert: (BehaviorTest correspondingForTest == Behavior).