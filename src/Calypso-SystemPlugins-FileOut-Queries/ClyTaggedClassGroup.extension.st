Extension { #name : 'ClyTaggedClassGroup' }

{ #category : '*Calypso-SystemPlugins-FileOut-Queries' }
ClyTaggedClassGroup >> fileOut [

	classQuery scope packagesDo: [ :each | (each tagNamed: self tag) fileOut ]
]
