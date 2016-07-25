class management
obsolete
	"The receiver is becoming obsolete. 
	NOTE: You if you remove the whole class category at once, you cannot
	assume that the ExternalType class is still present."

	Smalltalk at: #ExternalType ifPresent: [:class | class noticeRemovalOf: self].
	^ super obsolete