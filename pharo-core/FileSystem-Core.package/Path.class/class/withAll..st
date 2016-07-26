private
withAll: aCollection
	| inst |
	inst := self new: aCollection size.
	aCollection withIndexDo: [:segment :index |
		inst at: index put: segment].
	^ inst