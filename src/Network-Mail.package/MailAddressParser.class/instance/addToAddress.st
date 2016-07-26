building address list
addToAddress
	"add the last token to the address.  removes the token from the collection"
	curAddrTokens addFirst: (tokens removeLast)