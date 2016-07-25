*System-Finalization
doOldFinalization
	"Process the weak registries, in the old finalization style.  Hopefully this will
	 eventually go away when all clients have adopted the new finalization scheme."
	FinalizationLock critical: [
			FinalizationDependents do: 	[:weakDependent |
				weakDependent ifNotNil: [
					[ weakDependent finalizeValues ] on: Exception fork: [:ex | ex pass ] ]]].