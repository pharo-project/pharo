Extension { #name : #RBMethodNode }

{ #category : #'*Spec2-Inspector' }
RBMethodNode class >> additionalInspectorClasses [
	^ super additionalInspectorClasses, { EyeSyntaxTreeInspector } 
]
