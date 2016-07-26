dropping/grabbing
grabTransform
	"Return the transform for the receiver which should be applied during grabbing"
	self renderedMorph isWorldMorph 
		ifTrue:[^owner ifNil:[IdentityTransform new] ifNotNil:[owner grabTransform]].
	^owner ifNil:[self transform] ifNotNil:[owner grabTransform composedWithLocal: self transform]