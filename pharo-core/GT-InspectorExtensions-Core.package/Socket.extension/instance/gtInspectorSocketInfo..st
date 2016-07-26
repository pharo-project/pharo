*GT-InspectorExtensions-Core
gtInspectorSocketInfo: composite
	<gtInspectorPresentationOrder: 1>
	^ ((Dictionary new
		at: #isConnected put: self isConnected;
		at: #address put: self localAddress;
		at: #port put: self localPort;
		at: #isOtherEndClosed put: self isOtherEndClosed;
		at: #isThisEndClosed put: self isThisEndClosed;
		at: #isUnconnected put: self isUnconnected;
		at: #isUnconnectedOrInvalid put: self isUnconnectedOrInvalid;
		at: #isValid put: self isValid;
		at: #isWaitingForConnection put: self isWaitingForConnection;
		yourself)
			gtInspectorItemsIn: composite)
				title: 'Socket Info'