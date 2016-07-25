lookups
promptUserForHostAddressDefault: defaultName
	"Ask the user for a host name and return its address. If the default name is the empty string, use the last host name as the default."

	"NetNameResolver promptUserForHostAddressDefault: ''"

	| default hostName serverAddr |
	default := defaultName isEmpty
		ifTrue: [ DefaultHostName ]
		ifFalse: [ defaultName ].
	hostName := UIManager default request: 'Host name or address?' initialAnswer: default.
	hostName isEmptyOrNil
		ifTrue: [ ^ 0 ].
	serverAddr := NetNameResolver addressForName: hostName timeout: 15.
	hostName size > 0
		ifTrue: [ DefaultHostName := hostName ].
	^ serverAddr