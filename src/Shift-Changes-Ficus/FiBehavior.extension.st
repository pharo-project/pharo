Extension { #name : #FiBehavior }

{ #category : #'*Shift-Changes-Ficus' }
FiBehavior >> changeValidations [
	^ super changeValidations , ShFAbstractSingleChange allSubclasses
		,
			{ShFSlotChange.
			ShFClasspoolChange.
			ShFSharedPoolChange}
]
