copying
asSimulatedOblique
	^self copy 
		slantValue: LogicalFont slantItalic;  "treat italic and oblique the same"
		styleName: (fileInfo styleNameWithItalicForcedToBe: 'Oblique');
		simulated: true;
		yourself