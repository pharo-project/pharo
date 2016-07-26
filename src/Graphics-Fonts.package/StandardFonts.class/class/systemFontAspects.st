font accessing
systemFontAspects
	^ self systemFontAspectsAndLabels collect: [:assoc | assoc key]