accessing
predecessors: aPackage
	"returns a Collection with the predecessors of the package"

	| aCollection |
	aCollection := OrderedCollection new.
	self packages
		do: [ :package | 
			package dependencies
				do: [ :dependency | 
					dependency target = aPackage
						ifTrue: [ aCollection add: package ] ] ].
	^ aCollection