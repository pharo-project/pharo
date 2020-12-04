Extension { #name : #Morph }

{ #category : #'*EyeInspector' }
Morph class >> additionalInspectorClasses [
	^ super additionalInspectorClasses, { EyeViewHierarchyInspector. EyeMorphViewer }
]
