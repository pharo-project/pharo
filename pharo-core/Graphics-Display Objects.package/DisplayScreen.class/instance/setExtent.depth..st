private
setExtent: aPoint depth: bitsPerPixel  "DisplayScreen startUp"
	"This method is critical.  If the setExtent fails, there will be no
	proper display on which to show the error condition..."
	"ar 5/1/1999: ... and that is exactly why we check for the available display depths first."

	"RAA 27 Nov 99 - if depth and extent are the same and acceptable, why go through this.
	also - record when we change so worlds can tell if it is time to repaint"

	(depth == bitsPerPixel and: [aPoint = self extent and: 
					[self supportsDisplayDepth: bitsPerPixel]]) ifFalse: [
		bits := nil.  "Free up old bitmap in case space is low"
		DisplayChangeSignature := (DisplayChangeSignature ifNil: [0]) + 1.
		(self supportsDisplayDepth: bitsPerPixel)
			ifTrue:[super setExtent: aPoint depth: bitsPerPixel]
			ifFalse:[(self supportsDisplayDepth: bitsPerPixel negated)
				ifTrue:[super setExtent: aPoint depth: bitsPerPixel negated]
				ifFalse:["Search for a suitable depth"
					super setExtent: aPoint depth: self findAnyDisplayDepth]].
	].
	clippingBox := super boundingBox