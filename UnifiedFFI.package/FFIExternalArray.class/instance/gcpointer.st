converting
gcpointer
	"Answers a pointer to the this array. 
	 This is useful when translating an array pointer to a function (FFI requires to be passed as a
	 pointer, otherwise will be interpreted as a direct array and it will not work fine)
	
	 For an example, see FFICallback class>>#exampleCqsort
	
	 WARNING: Only valid for external address (you cannot have a pointer to an image side object)"
	self deprecated: 'Use "pointer autoRelease" instead.' on: '2016-01-22' in: #Pharo5.  		
	self getHandle isExternalAddress ifFalse: [ 
		self error: 'Arrays need to be moved to external memory space before passing them as pointers.' ].
	^ self getHandle gcpointer	