copying
contentsOfArea: aRect
 	"Return a new form which derives from the portion of the original form delineated by aRect."
	^self contentsOfArea: aRect 
		into: (self class extent: aRect extent depth: depth).