signaling
signalCurrentVersion: currentVersion streamVersion: streamVersion

	^ self signal: 'Unexpected stream version ', streamVersion asString, ' where it should be ', currentVersion asString, '.'
