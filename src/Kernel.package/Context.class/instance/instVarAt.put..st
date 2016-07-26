private
instVarAt: index put: value
	index = 3 
		ifTrue: [
			self stackp: value. 
			^ value].
	^ super instVarAt: index put: value