operations
classifyAll: aCollection under: heading

	aCollection do:
		[:element | self classify: element under: heading]