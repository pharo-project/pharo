Extension { #name : #SpMorphicCodeAdapter }

{ #category : #'*Spec2-Morphic-Backend-Tests' }
SpMorphicCodeAdapter >> hasSyntaxHighlightEnabled [

	^ (self widget textArea editingMode isKindOf: RubPlainTextMode) not
]
