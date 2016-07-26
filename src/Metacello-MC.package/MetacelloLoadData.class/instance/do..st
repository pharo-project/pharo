enumerating
do: aBlock

	self dataMap valuesDo: [:ar |
		aBlock value: (ar at: 1) value: (ar at: 2) value: (ar at: 3) ]