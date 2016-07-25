font accessing
systemFontLabelOf: aSelector
	^ self systemFontAspectsAndLabels
		detect: [ :assoc | assoc key = aSelector ]
		ifFound: [ :assoc | assoc value ]
		ifNone: [ '' ]