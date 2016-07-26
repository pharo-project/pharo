font accessing
systemFontLabels
	^ self systemFontAspectsAndLabels collect: [:assoc | assoc value]