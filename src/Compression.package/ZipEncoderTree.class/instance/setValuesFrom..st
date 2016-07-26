private
setValuesFrom: nodeList
	self bitLengths: (nodeList
			collect: [:n | n bitLength]
			from: 1
			to: maxCode + 1)
		codes: (nodeList
				collect: [:n | n code]
				from: 1
				to: maxCode + 1)