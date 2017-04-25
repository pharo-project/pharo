misc
lastError
	^ self ffiCall: #(int32 GetLastError(void))