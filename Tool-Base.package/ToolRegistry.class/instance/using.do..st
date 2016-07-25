misc
using: aToolName do: aBlock
	"Evaluate a block for tool found under corresponding name or silently ignore if its not found or nil"
	| tool |
	
	tool := tools at: aToolName ifAbsent: [ ^ self ].
	tool ifNotNil: [ ^ aBlock value: tool value ]