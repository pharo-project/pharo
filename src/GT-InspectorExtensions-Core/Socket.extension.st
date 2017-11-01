Extension { #name : #Socket }

{ #category : #'*GT-InspectorExtensions-Core' }
Socket >> gtInspectorSocketInfo: composite [
	<gtInspectorPresentationOrder: 1>
	^ composite with: [ :a | | dictionary |
		dictionary := Dictionary new.
		[ dictionary
			at: #isConnected put: self isConnected;
			at: #address put: self localAddress;
			at: #port put: self localPort;
			at: #isOtherEndClosed put: self isOtherEndClosed;
			at: #isThisEndClosed put: self isThisEndClosed;
			at: #isUnconnected put: self isUnconnected;
			at: #isUnconnectedOrInvalid put: self isUnconnectedOrInvalid;
			at: #isValid put: self isValid;
			at: #isWaitingForConnection put: self isWaitingForConnection.
		] on: NetworkError, PrimitiveFailed do: [ :exception | 
			dictionary at: #EXCEPTION put: exception ].
		(dictionary gtInspectorItemsIn: composite)
			title: [ 'Socket Info' ];
			yourself ]
]
