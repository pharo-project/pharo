copying
copyForm: srcForm to: destPt rule: rule
	^ self copyForm: srcForm to: destPt rule: rule
		colorMap: (srcForm colormapIfNeededFor: destForm)