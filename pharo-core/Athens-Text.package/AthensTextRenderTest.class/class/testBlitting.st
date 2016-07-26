as yet unclassified
testBlitting
	| fnt form blt |

	fnt := LogicalFont
	 familyName: 'Arial'
 	pointSize: 24.

	form := fnt realFont
		glyphOf: $W
				destDepth: 8
				colorValue: 1
				subpixelPosition: 0.


	blt := BitBlt toForm: Display.
	
	blt 
		sourceForm: form;
		destOrigin: 0@0;
		sourceOrigin: 0@0;
		halftoneForm: nil;
		combinationRule: 41;
		width: form width;
		height: form height;
		
		copyBitsColor: 0
		alpha: 255
		gammaTable:  FreeTypeSettings current gammaTable
		ungammaTable: FreeTypeSettings current gammaInverseTable.
		
"	blt copyForm: form to: 100@100 rule: 41."
	Display forceToScreen.
	
"
	Display getCanvas translucentImage: form at: 0@0.		

"
	
	
