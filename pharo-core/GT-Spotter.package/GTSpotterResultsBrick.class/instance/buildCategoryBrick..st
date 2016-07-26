adding
buildCategoryBrick: aProcessor
	"Create and initialize new container brick to contain candidates for aProcessor.
	Also creates a category title brick and assigns it to category brick to be
	added later to results brick as soon as there is more than one candidate"
	<return: #GTSpotterCategoryBrick>
	|titleBrick categoryBrick|
	
	"Initialize and style category title"
	titleBrick := self newTitleBrick on: self stepModel category: aProcessor.
	self themer spotterThemer categoryTitleStyleFor: titleBrick.
	
	"Initialize and style category container"
	categoryBrick := self newCategoryBrick on: self stepModel category: aProcessor.
	self themer spotterThemer categoryStyleFor: categoryBrick.
	
	"Assign title to category which will be added later to results brick"
	categoryBrick titleBrick: titleBrick.
	
	^ categoryBrick