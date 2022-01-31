Extension { #name : #RBSelectorEnvironment }

{ #category : #'*Manifest-Core' }
RBSelectorEnvironment >> smallLintCritics [
	
	| array |
			
	array := OrderedCollection new.
	self classes do: [:cl | 
		self selectorsForClass: cl do: [:sel | array add:  (cl>>sel)]]. 
			
	^ array
]
