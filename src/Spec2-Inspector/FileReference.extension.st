Extension { #name : #FileReference }

{ #category : #'*Spec2-Inspector' }
FileReference class >> additionalInspectorClasses [
	^ super additionalInspectorClasses, { EyeFileSystemInspector }
]
