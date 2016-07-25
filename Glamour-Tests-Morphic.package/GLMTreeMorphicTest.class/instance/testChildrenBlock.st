tests
testChildrenBlock
	| browser check |
	browser := GLMFinder new.
	check := false.
	browser show: [:a | a tree
		children: [ :each | 
			check := true.
			#() ]].
	window := browser openOn: ($a to: $d).
	World doOneCycle.
	self repeatAssert: [ check ] equals: true