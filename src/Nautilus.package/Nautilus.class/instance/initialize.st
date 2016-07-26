initialization
initialize
	super initialize.

	plugins := OrderedCollection new.
	self class pluginClasses do: [:each || class position |
		class := each first.
		position := each second.
		plugins add: ((class model: self) position: position; yourself)]