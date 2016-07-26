testing
isConnected
	"Dirty, but avoids fixing the plugin bug"

	[ NetNameResolver addressForName: 'www.esug.org' ]
		on: NameLookupFailure
		do: [ :exception | ^ false ].
	^ true