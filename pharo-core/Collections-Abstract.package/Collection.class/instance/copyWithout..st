copying
copyWithout: oldElement 
	"Answer a copy of the receiver that does not contain any
	elements equal to oldElement."

	^ self reject: [:each | each = oldElement]

"Examples:
	'fred the bear' copyWithout: $e
	#(2 3 4 5 5 6) copyWithout: 5
"