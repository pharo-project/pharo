accessing
needsSaving
	^ self modified or: [self requiredPackages anySatisfy: [:ea | ea workingCopy needsSaving]]