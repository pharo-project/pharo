adding
add: anObject finalizing: postMortemFinalizationTag
	"Register a new object in the table, with the associated tag. The object is held onto weakly, the tag--strongly. The tag will be passed to the owner as the argument of #finalize: some time after the object is garbage collected."

	| size start delta |
	accessProtect critical:
		[size := strongArray size.
		 start := firstUnusedIndex].
	start to: size do:
		[:i|
		(strongArray at: i) == nil ifTrue:
			[accessProtect critical:
				[weakArray at: i put: anObject.
				 strongArray at: i put: postMortemFinalizationTag.
				 firstUnusedIndex := i + 1.
				 i > lastUsedIndex ifTrue: [lastUsedIndex := i]].
			 ^anObject]].
	delta := size min: 4096.
	accessProtect critical:
		[(weakArray := weakArray grownBy: delta)
			 at: size + 1 put: anObject.
		 (strongArray := strongArray  grownBy: delta)
			at: size put: postMortemFinalizationTag.
		 firstUnusedIndex := size + 1.
		 lastUsedIndex := size].
	^anObject