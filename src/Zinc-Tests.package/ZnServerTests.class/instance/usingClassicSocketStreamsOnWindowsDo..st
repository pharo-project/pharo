private
usingClassicSocketStreamsOnWindowsDo: block
	| originalSocketStreamClass |
	originalSocketStreamClass := ZnNetworkingUtils default socketStreamClass.
	self runningOnWindows
		ifTrue: [ ZnNetworkingUtils default socketStreamClass: SocketStream ].
	block
		ensure: [ ZnNetworkingUtils default socketStreamClass: originalSocketStreamClass ]