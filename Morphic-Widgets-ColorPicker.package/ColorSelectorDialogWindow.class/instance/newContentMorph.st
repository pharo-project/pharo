actions
newContentMorph
	"Answer a new content morph."

	self hsvaMorph: self newHSVAColorSelectorMorph.
	^self newRow: {
		self newGroupbox: 'Color' translated forAll: {
			self hsvaMorph.
			(self newRow: {
				(self newLabelGroup: {
					'Selected color' translated -> self newColorPresenterMorph})
					vResizing: #shrinkWrap.
				self newColorPickerButtonMorph})
				cellPositioning: #leftCenter}.
		(self newGroupbox: 'Values' translated for: 
			(self newLabelGroup: {
				'Red' translated -> (self newColorComponentFieldMorph: #red).
				'Green' translated -> (self newColorComponentFieldMorph: #green).
				'Blue' translated -> (self newColorComponentFieldMorph: #blue).
				'Hue' translated -> (self newColorComponentFieldMorph: #hue).
				'Saturation' translated -> (self newColorComponentFieldMorph: #saturation).
				'Brightness' translated -> (self newColorComponentFieldMorph: #brightness).
				'Alpha' translated -> (self newColorComponentFieldMorph: #alpha)}))
			hResizing: #shrinkWrap}