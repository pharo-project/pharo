private
onError: aBlock do: errorBlock 
	^aBlock on: self class preconditionSignal
		do: 
			[:ex | 
			errorBlock value.
			ex return: nil]