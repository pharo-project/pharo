operations
load
	| loader |
	loader := MCPackageLoader new.
	loader provisions addAll: self provisions.
	self applyTo: loader.
	loader load