geometry
bounds: aRectangle in: referenceMorph
	"Return the receiver's bounds as seen by aMorphs coordinate frame"
	owner ifNil: [^ aRectangle].
	^(owner transformFrom: referenceMorph) localBoundsToGlobal: aRectangle
