accessing
categoryBrickOf: aProcessor ifAbsent: aBlock
	"Return a container brick that contains candidates for aProcessor.
	If category brick for aProcessor does not exist evaluate aBlock"
	<return: #GTSpotterCategoryBrick>
	
	^ self categories
		at: aProcessor
		ifAbsent: aBlock