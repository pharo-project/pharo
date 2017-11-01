Extension { #name : #RBMethodNode }

{ #category : #'*Spec-Inspector' }
RBMethodNode class >> additionalInspectorClasses [
	^ super additionalInspectorClasses, { EyeSyntaxTreeInspector } 
]
