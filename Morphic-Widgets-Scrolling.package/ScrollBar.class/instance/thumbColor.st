access
thumbColor
	"Problem: Part of the ScrollBar/Slider code uses 'slider' to mean the entire scrollbar/slider widget, and part of it uses 'slider' to mean only the draggable 'thumb'.  This should be cleaned up so that 'thumb' is used instead of 'slider' where appropriate.  For now, the meaning of thumbColor is clear, at least."

	^self class imageColorFor: self