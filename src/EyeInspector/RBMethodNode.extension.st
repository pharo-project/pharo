Extension { #name : #RBMethodNode }

{ #category : #'*EyeInspector' }
RBMethodNode class >> additionalInspectorClasses [
	^ super additionalInspectorClasses, { EyeSyntaxTreeInspector } 
]
