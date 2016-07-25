instance creation
newLabelFor: aModel label: aString getEnabled: enabledSel
	"Answer a new text label whose model is aModel"

	^ ( self contents: aString font: self labelFont )
		model: aModel;
		getEnabledSelector: enabledSel