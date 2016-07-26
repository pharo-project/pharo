private
methodsIn: class for: protocol

	^ (class methodsInProtocol: protocol)
		sorted: [ :a :b | a selector < b selector ]