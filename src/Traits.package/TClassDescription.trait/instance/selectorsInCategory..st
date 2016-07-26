accessing method dictionary
selectorsInCategory: aName 
	"Answer a list of the selectors of the receiver that are in category named aName"
	
	| aColl |
	aColl := Set withAll: (self organization listAtCategoryNamed: aName).
	^ aColl asArray sort