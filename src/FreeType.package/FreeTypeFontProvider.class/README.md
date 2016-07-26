This is a font provider for true type fonts. You can use it to add TTF files to your image:

FreeTypeFontProvider current 
	updateFontsFromSystem;
	updateAvailableFontFamilies. 

You can add TTF fonts from a spetial dirrectory:
FreeTypeFontProvider current 
	updateFromDirectory: './fonts/' asFileReference done: Set new.

Then you can use font dialog:
	FreeTypeFontSelectorDialogWindow new open.

Or set for example set ballon or default font as following:
	StandardFonts balloonFont: 
	        (LogicalFont familyName: 'Arial'  pointSize: 10). 
	StandardFonts defaultFont: 
	        (LogicalFont familyName: 'Arial'  pointSize: 10).