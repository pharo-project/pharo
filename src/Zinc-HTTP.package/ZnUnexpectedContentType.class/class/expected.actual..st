instance creation
expected: expectedMimeType actual: actualMimeType
	^ self new
		expectedContentType: expectedMimeType;
		actualContentType: actualMimeType;
		yourself