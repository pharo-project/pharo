converting
asFileUrl
	"Return a copy of the receiver as a File URL, replacing the scheme with #file.
	This is useful when you want to do a #asFileReference afterwards."
	
	^ self copy
		scheme: #file;
		yourself