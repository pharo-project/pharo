updating
updateReport
	self sourceTextModel setText: (Text streamContents: [:stream | 
						self categoryList do: [:each |
							(categoriesSelected includes: each) ifTrue: [
								self perform: ((categories at: each), ':') asSymbol with: stream.
								stream cr]]])
