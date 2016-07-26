framing
initialFrameFor: aView initialExtent: initialExtent world: aWorld 
	"Find a plausible initial screen area for the supplied view, which
	should be a StandardSystemView, taking into account the
	'reverseWindowStagger' Preference, the size needed, and other
	windows currently on the screen."
	
	^self perform: self usedStrategy with: aView with: initialExtent with: aWorld
	