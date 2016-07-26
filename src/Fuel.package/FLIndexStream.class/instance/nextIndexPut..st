accessing
nextIndexPut: v

	1 to: digits do: [:i | stream nextPut: (v digitAt: digits + 1 - i)]
