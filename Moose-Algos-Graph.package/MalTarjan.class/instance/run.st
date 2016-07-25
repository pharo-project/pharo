running
run
	sccs := OrderedCollection new.
	stack := Stack new.
	runningIndex := 0.
	self nodes
		do: [ :n | 
			n isTarjanUndefined
				ifTrue: [ self traverse: n ] ].
	^ sccs