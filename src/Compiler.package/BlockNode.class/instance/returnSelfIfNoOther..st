accessing
returnSelfIfNoOther: encoder

	self returns ifTrue:[^self].
	statements last == NodeSelf ifFalse: [
		statements := statements copyWith: (encoder encodeVariable: 'self').
	].
	self returnLast.
