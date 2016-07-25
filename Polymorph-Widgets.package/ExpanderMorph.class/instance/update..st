updating
update: aspect
	"Update the receiver."

	aspect = #expanded
		ifTrue: [self spaceFillWeight: (self expanded
					ifTrue: [1]
					ifFalse: [0]).
				self showMorphs: self expanded.
				self fixLayout.
				self expanded
					ifTrue: [self announce: (ExpanderMorphExpanded on: self)]
					ifFalse: [self announce: (ExpanderMorphContracted on: self)]]