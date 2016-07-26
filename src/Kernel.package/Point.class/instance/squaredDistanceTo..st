point functions
squaredDistanceTo: aPoint 
	"Answer the distance between aPoint and the receiver."
	| delta |
	delta := aPoint - self.
	^ delta dotProduct: delta