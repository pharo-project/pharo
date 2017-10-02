*Shift-Changes-Ficus
changeValidations
	^ super changeValidations , ShFAbstractSingleChange allSubclasses
		,
			{ShFSlotChange.
			ShFClasspoolChange.
			ShFSharedPoolChange}