textconstants access
defaultFamilyNames
	| DefaultTextStyle DefaultFixedTextStyle DefaultMultiStyle |

	DefaultTextStyle := TextSharedInformation at: #DefaultTextStyle.
	DefaultFixedTextStyle := TextSharedInformation at: #DefaultFixedTextStyle.
	DefaultMultiStyle := TextSharedInformation at: #DefaultMultiStyle.
	
	^#(DefaultTextStyle DefaultFixedTextStyle DefaultMultiStyle)