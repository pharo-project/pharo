Extension { #name : #CompiledMethod }

{ #category : #'*Spec2-Inspector' }
CompiledMethod class >> additionalInspectorClasses [
	^ super additionalInspectorClasses, { SpEyeMethodEditor }
]

{ #category : #'*Spec2-Inspector' }
CompiledMethod class >> inspectorClass [
	^ SpEyeCompiledMethodInspector
]
