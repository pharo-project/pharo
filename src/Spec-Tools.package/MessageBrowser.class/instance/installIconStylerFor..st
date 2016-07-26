private
installIconStylerFor: anItem
	"icons styler are only supported for method definitions (not nil / not for comment definitions (those may occure
	for comment or general string searches))"

	(self isMethodDefinition: anItem)
		ifFalse: [ ^ self ].
		
	IconStyler styleText: self textModel withAst: anItem ast.
	anItem annotateRubricText: self textModel