private
rehash 

	| newInstance |
	newInstance := self species newForCapacity: self basicSize.
	1 to: self basicSize do: [ :index | 
		(self basicAt: index) ifNotNil: [ :key |
			newInstance at: key put: (array at: index) ] ].
	self copyFrom: newInstance