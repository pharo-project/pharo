Extension { #name : 'MCDictionaryRepository' }

{ #category : '*Gofer-Deprecated' }
MCDictionaryRepository >> goferPriority [
	^ 10
]

{ #category : '*Gofer-Deprecated' }
MCDictionaryRepository >> goferReferences [
	^ self allVersionInfos collect: [ :each | GoferResolvedReference name: each name repository: self ]
]
