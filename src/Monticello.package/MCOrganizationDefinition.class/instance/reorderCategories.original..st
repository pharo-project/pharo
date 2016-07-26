private
reorderCategories: allCategories original: oldCategories
	| first |
	first := allCategories detect: [:ea | categories includes: ea] ifNone: [^ allCategories].
	^ 	((allCategories copyUpTo: first) copyWithoutAll: oldCategories, categories),
		categories,
		((allCategories copyAfter: first) copyWithoutAll: oldCategories, categories)
