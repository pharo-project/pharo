running
runWithMorphicDialogPopping: anAssertingBlock

	| semaphore worldSubmoprhs newMorphs closingProcess |

	semaphore := Semaphore new.

	worldSubmoprhs := World submorphs.
	closingProcess := [
		[ worldSubmoprhs = World submorphs ] whileTrue:  [ Processor yield ].
		newMorphs := (World submorphs difference: worldSubmoprhs).
		self assert: newMorphs size equals: 1. 
		newMorphs first close.
		semaphore signal ] fork.
	
	
	anAssertingBlock
		on: TestFailure
		do: [ :failure |
			closingProcess terminate.
			semaphore signal.
			failure pass. ].
		
	semaphore wait.