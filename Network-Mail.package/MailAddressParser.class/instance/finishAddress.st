building address list
finishAddress
	"we've finished one address.  Bundle it up and add it to the list of addresses"
	| address |

	address := String streamContents: [ :str |
		curAddrTokens do: [ :tok | str nextPutAll: tok text ] ].

	addresses addFirst: address.

	curAddrTokens := nil.