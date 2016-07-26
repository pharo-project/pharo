accessing
allowedArea
	"The area allowed for the building and positioning of the morph"
	| allowedArea |
	allowedArea := Display usableArea.
	allowedArea := allowedArea intersect: World visibleClearArea ifNone: [ 0@0 corner: 0@0 "not sure about this"].
	^allowedArea