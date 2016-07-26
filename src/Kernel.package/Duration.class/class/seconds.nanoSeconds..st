instance creation
seconds: seconds nanoSeconds: nanos
	^ self basicNew
		seconds: seconds truncated
		nanoSeconds: seconds fractionPart * NanosInSecond + nanos