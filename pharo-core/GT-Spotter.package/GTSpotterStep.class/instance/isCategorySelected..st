testing
isCategorySelected: aProcessor
	"Return true if a category assigned to aProcessor is currently selected,
	false otherwise"
	<return: #Boolean>
	
	^ self hasSelected and: [ self selected processor == aProcessor ]