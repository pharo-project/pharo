help text
shortcut: aString text: secondString on: aTextStream 
	aTextStream
		withAttributes: self shortcutAttributes
		do: [aTextStream nextPutAll: aString;
				 cr].
	aTextStream
		withAttributes: self explanationAttributes
		do: [aTextStream nextPutAll: secondString;
				 cr].
