private
selectedClass: aClass belongsToPackageSelection: aPackage
	"test whether the class belongs to a selected package. But if
we are in hierarchy view, the membership is more difficult to compute. Now, I just
assmume, if hierarchy view is enabled, selecting this class is what we want"

	^ (aPackage includesClass: aClass) or: [ self showHierarchy ]