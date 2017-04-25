class initialization
initializeSTONCharacters
	| escapes |
	STONCharacters := Array new: 127.
	32 to: 126 do: [ :each | 
		STONCharacters at: each + 1 put: #pass ].
	"This is the minimal STON set of named escapes"
	escapes := #( 8 '\b' 9 '\t' 10 '\n' 12 '\f' 13 '\r' 39 '\''' 92 '\\' ).
	escapes pairsDo: [ :code :escape | 
		STONCharacters at: code + 1 put: escape ]