initailize-release
sendMessage: aUnarySymbol
	"Initialze the receiver's collation block to compare the results of sending aUnarySymbol to each argument, and then collate them with the <=> method."

	collator := [ :a :b | self collate: (a perform: aUnarySymbol) with: (b perform: aUnarySymbol) ]