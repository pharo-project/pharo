accessing
extent: aPoint
	"Changed to not cause layoutChanged on the receiver."
	
	bounds extent = aPoint ifTrue: [^ self].
	self changed.
	bounds := (bounds topLeft extent: aPoint) rounded.
	super layoutChanged. "skip submorph bounds recalculation"
	self changed.
