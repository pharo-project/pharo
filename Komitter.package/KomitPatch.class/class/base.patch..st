instance creation
base: base patch: patch

	^ self new
		base: base;
		patch: patch;
		yourself