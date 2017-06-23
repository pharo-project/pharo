utilities
writeStreamFor: fileName in: directory do: aBlock 
	directory addEntryNamed: fileName withContents: (String streamContents: aBlock).