printing
sourceText

	^ String streamContents: [:stream |
		self printPrimitiveOn: stream]