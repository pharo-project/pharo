meta-actions
potentialTargets
	"Return the potential targets for the receiver.
	This is derived from Morph>>potentialEmbeddingTargets."
	owner ifNil:[^#()].
	^owner morphsAt: self referencePosition behind: self unlocked: true not