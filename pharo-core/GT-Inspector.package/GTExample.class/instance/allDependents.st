public
allDependents
	| examples |
	examples := Set new.
	(self traverser reversed
		action: [ :example | examples add: example ];
		yourself) value.
	^ examples