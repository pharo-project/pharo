accessing
categoryBrickOf: aProcessor
	"Return a container brick that contains candidates for aProcessor.
	If category brick for aProcessor does not exist throw an error"
	<return: #GTSpotterCategoryBrick>
	<throws: #Error when: 'Category for aProcessor does not exist'>
	
	^ self categories
		at: aProcessor
		ifAbsent: [ Error signal: 'Category for ', aProcessor asString, ' does not exist' ]