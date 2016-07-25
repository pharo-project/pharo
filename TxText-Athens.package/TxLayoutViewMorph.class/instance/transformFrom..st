morphic geometry
transformFrom: uberMorph
	"my local coordinate system is in sync with text layout"
	
	(owner isNil or: [self == uberMorph]) ifTrue: [^self transform].
	^(owner transformFrom: uberMorph) composedWithLocal: self transform