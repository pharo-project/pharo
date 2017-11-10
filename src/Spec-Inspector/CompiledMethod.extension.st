Extension { #name : #CompiledMethod }

{ #category : #'*Spec-Inspector' }
CompiledMethod class >> additionalInspectorClasses [
	^ super additionalInspectorClasses, { EyeMethodEditor }
]

{ #category : #'*Spec-Inspector' }
CompiledMethod class >> inspectorClass [
	^ EyeCompiledMethodInspector
]
