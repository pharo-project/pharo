private
updateDimension: aBrick

	"my height is already calculated, maybe because it children triggered calculation"
	aBrick isWidthClean ifTrue: [ ^ self ].
	(self dimensionPolicy: aBrick)  updateWidth: aBrick