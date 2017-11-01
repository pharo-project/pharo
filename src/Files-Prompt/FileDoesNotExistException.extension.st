Extension { #name : #FileDoesNotExistException }

{ #category : #'*Files-Prompt' }
FileDoesNotExistException >> defaultAction [
	"The default action taken if the exception is signaled."

	^ UIManager default fileDoesNotExistsDefaultAction: self

]
