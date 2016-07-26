public
apply: aPatch to: aSnapshot
	| loader |
	loader := self snapshot: aSnapshot.
	aPatch applyTo: loader.
	^ loader patchedSnapshot