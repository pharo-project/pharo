tests
tearDown

	Smalltalk globals at: #SystemProgressMorph ifPresent: [:spm | 
		spm uniqueInstance bars do: [ :e | e close ]] 