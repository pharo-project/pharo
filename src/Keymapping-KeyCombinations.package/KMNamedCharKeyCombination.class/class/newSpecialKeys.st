initialization
newSpecialKeys
"Build a new dictionary that maps untypable characters to symbols for instacation creation methods on this class. For now, only for SPACE Key"
	| dictionary |
	dictionary := Dictionary new.
	#(#space) do: [ :symbol | dictionary at: (Character perform: symbol) put: symbol ].
	^ dictionary