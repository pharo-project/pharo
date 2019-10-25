Extension { #name : #Class }

{ #category : #'*Reflectivity' }
Class >> intanceSpecificMetaLinksAvailable [
	^false
]

{ #category : #'*Reflectivity' }
Class >> link: aMetaLink toAST: aNode [
	aNode link: aMetaLink
]

{ #category : #'*Reflectivity' }
Class >> link: aMetaLink toClassVariable: aClassVariable [
	aClassVariable link: aMetaLink
]

{ #category : #'*Reflectivity' }
Class >> link: aMetaLink toClassVariableNamed: aClassVariableName [
	self
		link: aMetaLink
		toClassVariable: (MetalinkTargetResolver classVariableNamed: aClassVariableName for: self)
]

{ #category : #'*Reflectivity' }
Class >> link: aMetaLink toSlot: aSlot [
	aSlot link: aMetaLink
]

{ #category : #'*Reflectivity' }
Class >> link: aMetaLink toSlotNamed: aSlotName [
	self link: aMetaLink toSlot: (MetalinkTargetResolver slotNamed: aSlotName for: self)
]

{ #category : #'*Reflectivity' }
Class >> link: aMetaLink toTemporary: aTempVar [
	self flag: 'Not sure what it should do. The tests are not passing for this... See method testLinkClassToTempVarNamed'.
	aTempVar link: aMetaLink
]
