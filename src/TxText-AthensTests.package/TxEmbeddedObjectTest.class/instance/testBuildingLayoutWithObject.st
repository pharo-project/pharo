as yet unclassified
testBuildingLayoutWithObject
	(text startPosition moveRight: 5) insertObject: Object new.

	"null containers don't allow embedded objects"
	self should: [ view resetLayout ] raise: Error.

	