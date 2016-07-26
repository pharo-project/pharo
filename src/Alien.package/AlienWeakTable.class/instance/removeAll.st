removing
removeAll

	accessProtect critical:
		[1 to: lastUsedIndex do:
			[:index |
			strongArray at: index put: nil.
			weakArray at: index put: nil].
		firstUnusedIndex := 1.
		lastUsedIndex := 0]