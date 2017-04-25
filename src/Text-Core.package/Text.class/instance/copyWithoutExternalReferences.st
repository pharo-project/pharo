converting
copyWithoutExternalReferences

	^ self copy  
		removeAttributesThat: [:attr | attr mayHaveExternalReferences ] replaceAttributesThat: [:attr | false ] by: [:attr | ];
		yourself.