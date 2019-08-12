Extension { #name : #PluggableTextMorph }

{ #category : #'*Spec-MorphicAdapters' }
PluggableTextMorph >> registerScrollChanges: aSelector [

	self announcer 
		when: PaneScrolling 
		send: aSelector
		to: self model
]
