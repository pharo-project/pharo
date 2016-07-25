copying
asSimulatedBoldOblique
	^self copy 
		slantValue: LogicalFont slantItalic; "treat italic and oblique the same"
		weightValue:LogicalFont weightBold;
		styleName: (fileInfo styleNameWithWeightForcedToBe: 'Bold' italicForcedToBe: 'Oblique');
		simulated: true;
		yourself