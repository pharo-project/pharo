public
allDependencies
	| examples |
	examples := Set new.
	(self traverser
		action: [ :example | examples add: example ];
		yourself) value.
	^ examples