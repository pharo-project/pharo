Extension { #name : #CompiledMethod }

{ #category : #'*Spec2-Inspector' }
CompiledMethod class >> additionalInspectorClasses [
	^ super additionalInspectorClasses, { EyeMethodEditor }
]

{ #category : #'*Spec2-Inspector' }
CompiledMethod class >> inspectorClass [
	^ EyeCompiledMethodInspector
]
