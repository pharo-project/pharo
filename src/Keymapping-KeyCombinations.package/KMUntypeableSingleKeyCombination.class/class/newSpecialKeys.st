initialization
newSpecialKeys
	"Build a new dictionary that maps untypable characters to symbols for instacation creation methods on this class."
	| dictionary |
	dictionary := Dictionary new.
	 #(home enter end backspace tab pageUp pageDown escape arrowLeft arrowRight arrowUp arrowDown delete)
		do: [  :symbol |
			dictionary at: (Character perform: symbol) put: symbol ].
	^ dictionary