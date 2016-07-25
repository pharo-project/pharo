enumerating
reverseWith: aSequenceableCollection do: aBlock 
	"Evaluate aBlock with each of the receiver's elements, in reverse order, 
	along with the  corresponding element, also in reverse order, from 
	aSequencableCollection. "

	self size ~= aSequenceableCollection size ifTrue: [^ self errorSizeMismatch].
	self size to: 1 by: -1 do: [:index | 
			aBlock 
				value: (self at: index)
				value: (aSequenceableCollection at: index)]