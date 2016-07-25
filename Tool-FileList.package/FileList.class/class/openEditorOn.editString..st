morphic ui
openEditorOn: aFileStream editString: editString 
	"Open an editor on the given FileStream."
	^ (self openMorphOn: aFileStream editString: editString) openInWorld