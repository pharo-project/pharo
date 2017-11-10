Extension { #name : #MCDictionaryRepository }

{ #category : #'*gofer-core-accessing' }
MCDictionaryRepository >> goferPriority [
	^ 10
]

{ #category : #'*gofer-core-accessing' }
MCDictionaryRepository >> goferReferences [
	^ self allVersionInfos collect: [ :each | GoferResolvedReference name: each name repository: self ]
]
