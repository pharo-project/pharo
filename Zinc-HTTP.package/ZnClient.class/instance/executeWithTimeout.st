private protocol
executeWithTimeout
	^ self withTimeoutDo: [ 
		[ self executeWithRetriesRemaining: self numberOfRetries ]
			on: Error
			do: self ifFailBlock ]