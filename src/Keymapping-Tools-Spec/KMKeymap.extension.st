Extension { #name : 'KMKeymap' }

{ #category : '*Keymapping-Tools-Spec' }
KMKeymap >> scope [

	^ KMRepository default categories 
		detect: [ : cat | cat keymaps includes: self ]
]

{ #category : '*Keymapping-Tools-Spec' }
KMKeymap >> scopeName [

	^  self scope scopeName
]
