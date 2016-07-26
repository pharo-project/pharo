initialize-release
initialize

	| rng |
	rng := Random seed: 664399324.
	PermutationMap := (0 to: 4095) asArray shuffleBy: rng