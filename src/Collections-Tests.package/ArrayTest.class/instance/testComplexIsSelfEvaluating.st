tests
testComplexIsSelfEvaluating
	"self run: #testComplexIsSelfEvaluating"

	| complexArray restoredArray |
	complexArray := {1.
	true.
	false.
	nil.
	#a.
	'a'.
	$a.
	(Float pi).
	(Float halfPi).
	(4 / 5).
	(Float infinity negated).
	(1 @ 2).
	(0 @ 0 extent: 1 @ 1).
	('hola' -> 0).
	Object.
	(Object class)}.
	complexArray := complexArray copyWith: complexArray.
	self assert: complexArray isSelfEvaluating.
	restoredArray := self class evaluate: complexArray printString.
	self assert: restoredArray = complexArray