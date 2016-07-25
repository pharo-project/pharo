accessing
allEnabledSiblingItems
	"Answer the receiver's submorphs followed by the (wrapping) owner's
	submorph items. Answer only enabled items."

	^self allSiblingItems select: [:item | item isEnabled]