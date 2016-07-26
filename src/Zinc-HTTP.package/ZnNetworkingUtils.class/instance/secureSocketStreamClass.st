accessing
secureSocketStreamClass
	^ secureSocketStreamClass ifNil: [ 
		secureSocketStreamClass := Smalltalk globals at: #ZdcSecureSocketStream ifAbsent: [ nil ] ]