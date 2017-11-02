Extension { #name : #Morph }

{ #category : #'*Spec-Inspector' }
Morph class >> additionalInspectorClasses [
	^ super additionalInspectorClasses, { EyeViewHierarchyInspector. EyeMorphViewer }
]
