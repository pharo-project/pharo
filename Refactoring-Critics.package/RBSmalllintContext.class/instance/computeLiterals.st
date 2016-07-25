private
computeLiterals
	literalSemaphore := Semaphore new.
	literalProcess := [self primitiveComputeLiterals] fork