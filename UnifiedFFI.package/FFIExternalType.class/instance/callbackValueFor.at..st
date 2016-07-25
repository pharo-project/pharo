accessing
callbackValueFor: anObject at: index
	"This is the value for a callback. 
	 The callback parameters came from an external adress who can be treated as a ByteArray, so it 
	 works the same as an FFIExternalArray (at least for now)"
	^ self handle: anObject at: index