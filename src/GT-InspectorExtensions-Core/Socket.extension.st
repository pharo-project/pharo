Extension { #name : #Socket }

{ #category : #'*GT-InspectorExtensions-Core' }
Socket >> gtInspectorSocketInfo: composite [
	<gtInspectorPresentationOrder: 1>
	^ composite with: [ :a | | dictionary |
		dictionary := OrderedDictionary new.
		[ dictionary
			at: #status put: self statusString;
			at: #remoteAddress put: ($. join: self remoteAddress);
			at: #localAddress put: ($. join: self localAddress);
			at: #remotePort put: self remotePort;
			at: #localPort put: self localPort;
			at: #isConnected put: self isConnected;
			at: #isOtherEndClosed put: self isOtherEndClosed;
			at: #isThisEndClosed put: self isThisEndClosed;
			at: #isUnconnected put: self isUnconnected;
			at: #isUnconnectedOrInvalid put: self isUnconnectedOrInvalid;
			at: #isValid put: self isValid;
			at: #isWaitingForConnection put: self isWaitingForConnection.
		] on: NetworkError, PrimitiveFailed do: [ :exception | 
			dictionary at: #EXCEPTION put: exception ].
		(dictionary gtInspectorItemsIn: composite)
			title: 'Socket Info';
			yourself ]
]
