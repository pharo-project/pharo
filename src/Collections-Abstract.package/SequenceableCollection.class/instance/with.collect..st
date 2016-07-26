enumerating
with: otherCollection collect: twoArgBlock 
	"Collect and return the result of evaluating twoArgBlock with corresponding elements from this collection and otherCollection."
	| result |
	otherCollection size = self size ifFalse: [self errorSizeMismatch].
	result := self species new: self size.
	1 to: self size do:
		[:index | result at: index put:
		(twoArgBlock
			value: (self at: index)
			value: (otherCollection at: index))].
	^ result