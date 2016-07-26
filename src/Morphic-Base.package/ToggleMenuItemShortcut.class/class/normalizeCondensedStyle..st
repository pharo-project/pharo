private
normalizeCondensedStyle: aString
	"Assuming a keyText 'Old Style': the menu keys will come as 'o, k'"
	^ String streamContents: [ :stream | | keys |
		keys := (aString substrings: ',') collect: [ :each | each trimBoth ].
		stream << OSPlatform current menuShortcutString capitalized << ' + '.
		keys 
			do: [ :each | 
			(each first isUppercase) ifTrue: [ stream << 'Shift + ' ].
			stream << each asLowercase capitalized ]
			separatedBy: [ stream << ' + ' ] ]