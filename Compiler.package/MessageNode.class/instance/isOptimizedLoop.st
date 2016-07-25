testing
isOptimizedLoop
	^special > 0
	   and: [#(transformWhile: transformToDo:) includes: (MacroTransformers at: special)]