Extension { #name : 'KMCategory' }

{ #category : '*Keymapping-Tools-Spec' }
KMCategory >> scopeName [

	^  self name
]

{ #category : '*Keymapping-Tools-Spec' }
KMCategory >> shortcuts [

	^ (self entriesAt: #all) keymaps
]
