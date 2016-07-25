text area font
fontPointSize: pointSize
	self textArea addAttribute: (TextFontChange fontNumber: (self textStyle fontIndexOfPointSize: pointSize))