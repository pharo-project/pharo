asserting
assertPackageMorph: eachMorph
	| tagMorph tag |
	self assert: eachMorph isExpanded.
	eachMorph children ifEmpty: [ ^ self ]. 
	tag := compiledMethod package classTagForClass: compiledMethod methodClass.
	tagMorph := eachMorph children select: [ :eachChild | eachChild complexContents item = tag ].
	self assert: tagMorph size equals: 1.
	^ self assert: tagMorph first isSelected