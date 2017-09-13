results
associationForClass
	| assoc |
	assoc := self environment associationAt: class name ifAbsent: [nil].
	^assoc value == class
		ifTrue: [assoc]
		ifFalse: [Association new value: class]