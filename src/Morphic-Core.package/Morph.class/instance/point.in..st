geometry
point: aPoint in: aReferenceMorph

	owner ifNil: [^ aPoint].
	^ (owner transformFrom: aReferenceMorph) localPointToGlobal: aPoint.
