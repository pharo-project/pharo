tests
testSpawnTabulator

	| browser |
	browser := GLMTabulator new.
	window := browser openOn: ($a to: $d).
	self assert: window model == browser.
	self repeatAssert: [
		window submorphs last 
					submorphs last 
						submorphs last 
							submorphs last class] equals: GLMPaginatedMorphTreeMorph
