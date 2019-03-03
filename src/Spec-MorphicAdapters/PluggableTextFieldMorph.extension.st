Extension { #name : #PluggableTextFieldMorph }

{ #category : #'*Spec-MorphicAdapters' }
PluggableTextFieldMorph >> setTextAndAccept: aText [
	self
		setText: aText;
		hasUnacceptedEdits: true;
		accept
]
