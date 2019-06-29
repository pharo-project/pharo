Extension { #name : #Morph }

{ #category : #'*Spec2-Inspector' }
Morph class >> additionalInspectorClasses [
	^ super additionalInspectorClasses, { EyeViewHierarchyInspector. EyeMorphViewer }
]
