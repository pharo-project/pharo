private
internalize
	"Copy the data of this object over to Smalltalk memory if it is currently external.
	Now we can keep on using this object after the containing object (Blob, Tree or Commit)
	has been freed."

	(handle isExternalAddress and: [ handle isNull not ])
		ifTrue: [ 
			| bytes |
			bytes := handle copyFrom: 1 to: self class byteSize.
			handle := bytes ]