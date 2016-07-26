signaling
signalCurrentSignature: currentSignature streamSignature: streamSignature

	^ self signal: 'Unexpected stream signature ', streamSignature asString, ' where it should be ', currentSignature asString, '.'