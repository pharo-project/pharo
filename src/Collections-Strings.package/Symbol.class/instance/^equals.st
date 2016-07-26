comparing
= aSymbol
	"Compare the receiver and aSymbol." 
	self == aSymbol ifTrue: [^ true].
	self class == aSymbol class ifTrue: [^ false].
	"Use String comparison otherwise"
	^ super = aSymbol