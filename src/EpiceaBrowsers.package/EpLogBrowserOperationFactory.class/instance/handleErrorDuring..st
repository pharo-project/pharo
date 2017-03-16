private
handleErrorDuring: aBlock

	aBlock
		on: Error
		do: self errorHandlerBlock
