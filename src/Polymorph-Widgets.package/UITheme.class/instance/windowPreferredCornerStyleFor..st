defaults
windowPreferredCornerStyleFor: aWindow
	"Answer the preferred corner style for the given window."

	aWindow isEmbedded ifTrue: [^#square].
	^(self settings preferRoundCorner  or: [
			aWindow cornerStyle == #rounded])
		ifTrue: [#rounded]
		ifFalse: [#square]