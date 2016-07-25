initialization
highlightError.
	"copy in the error message"
	contents := contents
					copyReplaceFrom: location
					to: location - 1
					with: self sourceErrorString.
	"convert to Text that we can add Attributes"
	contents := contents asText.
	"use the ST80Styler"
	location ifNil: [ ^ self ].	
	contents addAttribute: TextColor red from: location to: location + self sourceErrorString size - 1.
	contents addAttribute: TextEmphasis bold from: location to: location + self sourceErrorString size - 1.