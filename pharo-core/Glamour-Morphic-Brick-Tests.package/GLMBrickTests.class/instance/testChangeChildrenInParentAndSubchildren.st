tests-layouter
testChangeChildrenInParentAndSubchildren
	|parent brick child|
	parent := self newBrickWithFullDependsOnChildren.
	brick := self newBrickWithStaticExtent.
	child := self newBrickFullHeightAndWidthDependsOnParent: 100.
	
	parent addBrickBack: brick.
	brick addBrickBack: child.
	self assertAllSubbricksAreClean: parent.
	self assert: parent extent equals: brick extent.
	self assert: child extent equals: brick extent.
	"height and width are changed, so parent should be re-layouted"
	brick height: self staticHeight * 2.
	brick width: self staticWidth * 2.
	
	self assertAllSubbricksAreClean: parent.
	self assert: brick extent equals: self staticExtent * 2.
	self assert: parent extent equals: brick extent.
	self assert: child extent equals: brick extent
	
