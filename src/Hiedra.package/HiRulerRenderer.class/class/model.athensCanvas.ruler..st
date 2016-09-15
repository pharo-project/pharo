instance creation
model: aModel athensCanvas: aCanvas ruler: aRuler
	^ self basicNew
		initializeWithModel: aModel
		athensCanvas: aCanvas
		ruler: aRuler;
		yourself