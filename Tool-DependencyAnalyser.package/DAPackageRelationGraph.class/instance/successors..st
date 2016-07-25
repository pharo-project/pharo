accessing
successors: aPackage
	"answer a collection with the successors of aPackage in the graph"

	| aCollection |
	aCollection := OrderedCollection new.
	(packages at: aPackage packageName) dependencies
		do: [ :dependency | 
			(self packagesName includes: dependency target packageName)
				ifTrue: [ aCollection add: dependency target ] ].
	^ aCollection