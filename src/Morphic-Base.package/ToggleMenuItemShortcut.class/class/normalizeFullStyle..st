private
normalizeFullStyle: aString
	"Assuming is a keytext with the format: 'cmd+shift+something'"
	^ String streamContents: [ :stream | 
			((aString substrings: '+') collect: #trimBoth)
				do: [ :each | stream << (each asLowercase capitalized) ]
				separatedBy: [ stream << ' + ' ] ]	