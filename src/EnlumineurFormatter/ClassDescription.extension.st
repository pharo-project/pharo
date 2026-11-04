Extension { #name : 'ClassDescription' }

{ #category : '*EnlumineurFormatter' }
ClassDescription >> reformatAll [
	"Reformat the methods I define. Those I have from traits are left to their traits."

	self localMethods do: [ :method | method reformat ]
]
