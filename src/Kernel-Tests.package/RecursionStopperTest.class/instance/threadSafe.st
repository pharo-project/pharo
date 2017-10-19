accessing
threadSafe

	| sema |
	
	RecursionStopper during: [ 
		self assert: fork isNil.
		sema := Semaphore new.
		
		fork := [ self threadSafe. sema signal ] fork.	
		
		sema wait.
		self assert: fork isTerminated.
			
		fork := nil.
		
	].	
