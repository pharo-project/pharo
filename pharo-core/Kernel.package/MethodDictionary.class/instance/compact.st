private
compact
	"Make sure that I have the highest possible load factor (between 37.5% and 75%)."
	
	| newInstance |
	newInstance := self compactWithoutBecome.
	newInstance capacity = self capacity
		ifTrue: [ self copyFrom: newInstance ]
		ifFalse: [ self becomeForward: newInstance ]