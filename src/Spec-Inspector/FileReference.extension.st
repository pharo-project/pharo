Extension { #name : #FileReference }

{ #category : #'*Spec-Inspector' }
FileReference class >> additionalInspectorClasses [
	^ super additionalInspectorClasses, { EyeFileSystemInspector }
]
