private
updatePackageGroupAndClassList
	| selectedPackage selectedClass |
	
	
	selectedPackage := self selectedPackage.
	selectedClass := self selectedClass.
	super updatePackageGroupAndClassList.
	"I need to restore selection (otherwise, nothing goes to the right position)"
	self packageWidget updateList.
	self selectedPackage: selectedPackage.
	(selectedPackage isNil or: [ selectedClass isNil ])
		ifTrue: [ ^ self ].
	"only reselect the former class"
	"if it is still in this package"
	(self selectedClass: selectedClass belongsToPackageSelection: selectedPackage)
		ifTrue: [ self selectedClass: selectedClass ]