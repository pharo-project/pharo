dropping/grabbing
grabTransform
	"Return the transform for the receiver which should be applied during grabbing"
	^owner ifNil:[self transform] ifNotNil:[owner grabTransform composedWithLocal: self transform]