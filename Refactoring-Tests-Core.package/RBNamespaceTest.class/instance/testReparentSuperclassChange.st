class tests
testReparentSuperclassChange
	| st superclass subclasses |
	st := RBNamespace new.
	superclass := st classFor: TestCase superclass.
	subclasses := TestCase subclasses collect: [:each | st classFor: each].
	st reparentClasses: subclasses to: superclass.
	subclasses do: [:each | self assert: each superclass = superclass]