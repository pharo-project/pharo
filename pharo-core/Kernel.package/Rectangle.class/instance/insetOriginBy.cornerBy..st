rectangle functions
insetOriginBy: originDeltaPoint cornerBy: cornerDeltaPoint 
	"Answer a Rectangle that is inset from the receiver by a given amount in 
	the origin and corner."

	^Rectangle
		origin: origin + originDeltaPoint
		corner: corner - cornerDeltaPoint