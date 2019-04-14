Extension { #name : #SugsBrowseClassReferences }

{ #category : #'*ClassParser' }
SugsBrowseClassReferences class >> nodes [
	^{RBGlobalNode .  CDClassDefinitionNode . CDMetaclassDefinitionNode }
]
