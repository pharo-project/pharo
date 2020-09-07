Extension { #name : #FileReference }

{ #category : #'*EyeInspector' }
FileReference class >> additionalInspectorClasses [
	^ super additionalInspectorClasses, { EyeFileSystemInspector }
]
