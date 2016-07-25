private
cleanUpInstanceVariables
	self class allInstVarNames do: [ :name |
		name = 'testSelector' ifFalse: [
			self instVarNamed: name put: nil ] ]