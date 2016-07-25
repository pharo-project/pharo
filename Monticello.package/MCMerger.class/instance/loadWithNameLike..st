operations
loadWithNameLike: baseName
	| loader |
	loader := MCPackageLoader new.
	loader provisions addAll: self provisions.
	self applyTo: loader.
	loader loadWithNameLike: baseName