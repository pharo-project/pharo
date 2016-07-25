drawing
newContentMorph
	"Answer a new content morph"
	
	| pt |
	pt := PluggableTextFieldMorph new
		convertTo: String;
		alwaysAccept: true;
		on: self
		text: #content
		accept: #content:
		readSelection: nil
		menu: nil;
		acceptOnCR: true;
		getEnabledSelector: nil;
		font: self theme textFont;
		cornerStyle: (self theme textEntryCornerStyleIn: self);
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		borderStyle: (BorderStyle simple width: 0);
		hideScrollBarsIndefinitely;
		setBalloonText: nil.
		
	pt textMorph autoFit: true;
		wrapFlag: false;
		margins: self theme editableDropListMargins.
		
	^ pt