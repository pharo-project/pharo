accessing
categoryBrickFor: aProcessor
	"Return a container brick that contains candidates for aProcessor.
	If category brick for aProcessor is requested for the first time
	it gets created without being added to spotter UI"
	<return: #GTSpotterCategoryBrick>
	
	^ self categories
		at: aProcessor
		ifAbsentPut: [ self buildCategoryBrick: aProcessor ]