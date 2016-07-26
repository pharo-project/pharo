color mapping
makeBWForm: foregroundColor
	"Map this form into a B/W form with 1's in the foreground regions."
	| bwForm map |
	bwForm := Form extent: self extent.
	map := self newColorMap.  "All non-foreground go to 0's"
	map at: (foregroundColor indexInMap: map) put: 1.
	bwForm copyBits: self boundingBox from: self at: 0@0 colorMap: map.
	^ bwForm