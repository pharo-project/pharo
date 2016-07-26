printing
printElementsOn: aStream
	"The original code used #skip:, but some streams do not support that,
	 and we don't really need it."

	aStream nextPut: $(.
	self do: [:element | aStream print: element] separatedBy: [aStream space].
	aStream nextPut: $)