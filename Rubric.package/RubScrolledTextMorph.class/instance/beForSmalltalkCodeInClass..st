initialize-release
beForSmalltalkCodeInClass: aClass
	self textArea beForSmalltalkCodeInClass: aClass.
	self withTextSegmentIcons.
	self textArea font: StandardFonts codeFont