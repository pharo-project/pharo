protocol
removeLast
	" I remove the less seen one, in case of equality, I remove the latest one "


	| min reversedStorage |
	reversedStorage := storage reverse.
	min := reversedStorage first.
	2 to: storage size do: [:index || each |
		each := reversedStorage at: index.
		(each value < min value)
			ifTrue: [ min := each ]].
	storage remove: min