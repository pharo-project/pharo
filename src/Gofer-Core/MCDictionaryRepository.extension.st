Extension { #name : 'MCDictionaryRepository' }

{ #category : '*Gofer-Core' }
MCDictionaryRepository >> goferPriority [
	^ 10
]

{ #category : '*Gofer-Core' }
MCDictionaryRepository >> goferReferences [
	^ self allVersionInfos collect: [ :each | GoferResolvedReference name: each name repository: self ]
]
