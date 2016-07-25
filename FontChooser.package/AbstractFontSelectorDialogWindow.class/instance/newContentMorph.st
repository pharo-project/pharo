actions
newContentMorph
	"Answer a new content morph."

	self textPreviewMorph: self newTextPreviewMorph.
	^(self newColumn: {
		(self newRow: {
			self newGroupbox: 'Family' translated for:
				self newFontFamilyMorph.
			(self newColumn: {
				(self newGroupbox: 'Style' translated for: 
					self newFontStyleButtonRowMorph)
					vResizing: #shrinkWrap.
				self newGroupbox: 'Point size' translated for:
					self newFontSizeMorph})
				hResizing: #shrinkWrap})
			vResizing: #spaceFill.
		(self newGroupbox: 'Preview' translated for:
			self textPreviewMorph)
			vResizing: #shrinkWrap})
		minWidth: 350;
		minHeight: 400