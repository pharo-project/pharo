finalization
finalizeValues
	"Sent periodically by the finalization loop to check for any dead objects and report their death to the owner."
	
	accessProtect critical:
		[| usedIndex value newLastUsedIndex |
		 newLastUsedIndex := 0.
		 1 to: lastUsedIndex do:
			[:i|
			(value := strongArray at: i) ifNotNil:
				[(weakArray at: i)
					ifNil:
						[strongArray at: i put: nil.
						 owner finalize: value.
						 firstUnusedIndex > i ifTrue:
							[firstUnusedIndex := i]]
					ifNotNil: [newLastUsedIndex := i]]].
		 lastUsedIndex := newLastUsedIndex]