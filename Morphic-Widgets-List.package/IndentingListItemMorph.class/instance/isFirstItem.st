accessing
isFirstItem
	"I have no idea why the owner of the list can get nil but it happens when the packages are published with the Monticello Browser."
	
	^ (owner ~= nil) and: [owner submorphs first == self]