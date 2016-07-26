packages
allClassesInPackageNamed: packageName
	"Returns the list of classes that belongs to a Monticello package."
	
	"It would be probably better to delegate that query to the package itself"
	| classes organization |
	organization := self environment organization.
	classes := (organization classesInCategory: packageName) asSet.
	(organization categoriesMatching: packageName, '-*') do: [:category| 
		classes addAll: (organization classesInCategory: category)].
	^ classes asArray