Extension { #name : #MorphicCodeAdapter }

{ #category : #'*Spec-BackendTests' }
MorphicCodeAdapter >> hasSyntaxHighlightEnabled [
	^ (self widget textArea editingMode isKindOf: RubPlainTextMode) not
]
