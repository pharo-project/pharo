geometry
bounds: aRectangle from: referenceMorph
	"Return the receiver's bounds as seen by aMorphs coordinate frame"
	owner ifNil: [^ aRectangle].
	^(owner transformFrom: referenceMorph) globalBoundsToLocal: aRectangle
