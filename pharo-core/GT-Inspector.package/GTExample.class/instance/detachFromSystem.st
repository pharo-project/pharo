private
detachFromSystem
	self detachResultFromSystem.
	(self traverser reversed
		action: [ :example | example detachResultFromSystem ];
		onRecursionError: [ :aProcessor :anExample | " ignore and continue " ];
		yourself) value.
	children do: [ :proxy | proxy detachFromSystem ].
	parents do: [ :proxy | proxy detachFromSystem ].
	children := OrderedCollection new.
	parents := Set new.