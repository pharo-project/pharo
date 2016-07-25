accessing
transformationOfTrait: aTrait
	"Return the transformation which holds aTrait
	or nil if this composition doesn't include aTrait."
	
	^self transformations
		detect: [:each | each trait = aTrait]
		ifNone: [nil]