private
updateDimension: aBrick
	"my height is already calculated, maybe because my children triggered calculation"
	aBrick isHeightClean ifTrue: [ ^ self ].
	
	(self dimensionPolicy: aBrick) updateHeight: aBrick