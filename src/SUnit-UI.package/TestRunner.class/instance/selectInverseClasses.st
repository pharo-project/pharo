accessing-classes
selectInverseClasses
	"Fixed to update all selections now that the
	selection invalidation has been optimised."
	
	classesSelected := classes asSet 
		removeAll: classesSelected;
		yourself.
	self
		changed: #allSelections;
		changed: #classSelected;
		changed: #hasRunnable