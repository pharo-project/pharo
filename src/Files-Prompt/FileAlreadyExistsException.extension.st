Extension { #name : #FileAlreadyExistsException }

{ #category : #'*Files-Prompt' }
FileAlreadyExistsException >> defaultAction [
	"The default action taken if the exception is signaled."

	^ UIManager default fileExistsDefaultAction: self
]
