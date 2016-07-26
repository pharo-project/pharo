transforming
changeIsKindOfReferences
	| replacer |
	replacer := RBParseTreeRewriter new.
	replacer replace: '``@object isKindOf: ' , parent name
		with: '``@object isKindOf: ' , className.
	self convertAllReferencesToClass: parent using: replacer