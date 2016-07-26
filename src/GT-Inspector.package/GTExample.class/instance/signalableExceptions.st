public
signalableExceptions
	^ self exceptions copyWithAll: { Halt. Error. Exit. UnhandledError. TestFailure. Deprecation. }