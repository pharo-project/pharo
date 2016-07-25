private
primitiveExecute
	definedSelector := self changeClass 
		compile: self source
		classified: self protocol
		notifying: self controller