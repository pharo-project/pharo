accessing-classes
selectAllClasses
	"Fixed to update all selections now that the
	selection invalidation has been optimised."
	
	classesSelected := classes asSet.
	self
		changed: #allSelections;
		changed: #classSelected;
		changed: #hasRunnable