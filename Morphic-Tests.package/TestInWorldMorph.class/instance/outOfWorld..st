accessing
outOfWorld: aWorld
	aWorld ifNil:[^self].
	super outOfWorld: aWorld.
	outOfWorldCount := outOfWorldCount + 1.
