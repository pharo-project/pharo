private
browse: anAbstractFileReference

	| tokens testClass |
	anAbstractFileReference ifNil: [ ^self ].
	tokens := anAbstractFileReference base findTokens: '-'.
	testClass := Smalltalk at: tokens first asSymbol.
	Smalltalk tools browser openOnClass: testClass selector: tokens second asSymbol.