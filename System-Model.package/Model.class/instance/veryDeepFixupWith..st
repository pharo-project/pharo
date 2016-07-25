copying
veryDeepFixupWith: deepCopier 
	"See if the dependents are being copied also.  If so, point at the new copies.  (The dependent has self as its model.)
	Dependents handled in class Object, when the model is not a Model, are fixed up in Object veryDeepCopy."

	| originalDependents refs |
	super veryDeepFixupWith: deepCopier.
	originalDependents := dependents.
	originalDependents ifNil: [
		^self.
		].
	dependents := nil.
	refs := deepCopier references.
	originalDependents
		do: [:originalDependent | | newDependent | 
			newDependent := refs
						at: originalDependent
						ifAbsent: [].
			newDependent
				ifNotNil: [self addDependent: newDependent]]