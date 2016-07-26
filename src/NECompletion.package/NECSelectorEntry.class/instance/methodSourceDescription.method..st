private
methodSourceDescription: aClass method: aCompiledMethod 
	| styler styledText |
	
	styler := SHTextStylerST80 new.
	styler classOrMetaClass: aClass.
	styledText := styler styledTextFor: (aCompiledMethod sourceCode) asText.
	
	^ NECEntryDescription
		label: self label
		title: aClass printString
		description: styledText