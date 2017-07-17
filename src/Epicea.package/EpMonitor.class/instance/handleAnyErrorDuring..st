private
handleAnyErrorDuring: aBlock

	aBlock on: Error do: [:error | self handleError: error ]