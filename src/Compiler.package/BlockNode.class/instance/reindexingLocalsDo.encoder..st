code generation (closures)
reindexingLocalsDo: aBlock encoder: encoderOrNil
	"Evaluate aBlock wih arguments, temporaries and copiedValues reindexed for
	 their positions within the receiver's block, restoring the correct indices afterwards.
	 If encoder is not nil remember the temps for this block's extent."
	| tempIndices result tempsToReindex |
	[copiedValues notNil] assert.
	tempsToReindex := arguments asArray, copiedValues, temporaries.
	tempIndices := tempsToReindex collect: [:temp| temp index].
	tempsToReindex withIndexDo:
		[:temp :newIndex| temp index: newIndex - 1. [temp index + 1 = newIndex] assert].
	encoderOrNil ifNotNil:
		[encoderOrNil noteBlockExtent: blockExtent hasLocals: tempsToReindex].
	result := aBlock ensure:
				["Horribly pragmatic hack.  The copiedValues will have completely
				  unrelated indices within the closure method and sub-method.
				  Avoiding the effort of rebinding temps in the inner scope simply
				  update the indices to their correct ones during the generation of
				  the closure method and restore the indices immedately there-after."
				 tempsToReindex with: tempIndices do:
					[:temp :oldIndex| temp index: oldIndex. [temp index = oldIndex] assert]].
	^result