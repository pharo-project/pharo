initialization
loadAncestorsAndStepChildrenFrom: aVersionInfo
	"Make this non-lazy if we have some version information."

	ancestors := WeakArray withAll: aVersionInfo ancestors.
	stepChildren := WeakArray withAll: aVersionInfo stepChildren