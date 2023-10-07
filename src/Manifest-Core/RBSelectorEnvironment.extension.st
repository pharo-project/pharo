Extension { #name : 'RBSelectorEnvironment' }

{ #category : '*Manifest-Core' }
RBSelectorEnvironment >> smallLintCritics [

	| array |
	array := OrderedCollection new.
	self classes do: [ :class | self methodsForClass: class do: [ :method | array add: method ] ].
	^ array
]
