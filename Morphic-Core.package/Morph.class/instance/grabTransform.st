dropping/grabbing
grabTransform
	"Return the transform for the receiver which should be applied during grabbing"
	^owner ifNil:[IdentityTransform new] ifNotNil:[owner grabTransform]