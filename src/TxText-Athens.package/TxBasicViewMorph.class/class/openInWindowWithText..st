examples
openInWindowWithText: aTxModel
	"PLEASE NOTE!!! This method is not part of official API.
	used only for examples"
 
	| editorMorph wrapper |
	editorMorph := self text: aTxModel.
	editorMorph
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		layoutFrame: LayoutFrame identity;
		wrapText: true.
		
	wrapper := AthensWrapMorph new.
	wrapper
		layoutPolicy: ProportionalLayout new;
		addMorph: editorMorph.
	
	^ wrapper openInWindow