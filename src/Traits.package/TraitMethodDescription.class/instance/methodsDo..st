enumeration
methodsDo: aBlock
	self locatedMethods do: [:each |
		aBlock value: each ]