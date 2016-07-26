items creation
buildRegExChooser
	| string btnHeight btnWidth |
	string := 'Regexp'.
	btnHeight := StandardFonts buttonFont height + 8.
	btnWidth := 0.
	string do: [:c | btnWidth := btnWidth + (StandardFonts buttonFont widthOf: c)].
	^useRegExCheckbox := (CheckboxMorph 
									on: self finder
									selected: #useRegEx
									changeSelected: #useRegEx:)
									beCheckbox;
									vResizing: #rigid;
									height: btnHeight;
									width: btnWidth + 30;
									label: string