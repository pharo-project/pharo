protected
verifyVersionFrom: aDecoder

	| streamVersion |
	streamVersion := aDecoder nextEncodedUint16.
	(self version = streamVersion) ifFalse: [ 
		FLBadVersion 
			signalCurrentVersion: self version 
			streamVersion: streamVersion ]
