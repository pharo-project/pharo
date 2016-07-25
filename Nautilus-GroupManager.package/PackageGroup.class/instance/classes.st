queries
classes
	| classes |
	classes := OrderedCollection new.
	packages do: [ :package | package classes do: [ :class | classes addIfNotPresent: class ] ].
	^ classes