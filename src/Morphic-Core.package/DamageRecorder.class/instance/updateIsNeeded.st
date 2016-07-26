testing
updateIsNeeded
	"Return true if the display needs to be updated."

	^totalRepaint or: [invalidRects notEmpty]