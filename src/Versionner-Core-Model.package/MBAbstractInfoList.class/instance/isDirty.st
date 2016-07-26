testing
isDirty
	^ self packages anySatisfy: [:each | each isDirty ]