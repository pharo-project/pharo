installing
createClass
	| trait |
	trait := Trait
		named: name
		uses: (Smalltalk compiler evaluate: self traitCompositionString)
		category: category.
	trait ifNotNil: [trait classComment: comment stamp: commentStamp].
	^trait