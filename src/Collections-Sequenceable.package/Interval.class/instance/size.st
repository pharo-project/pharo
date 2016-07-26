accessing
size
	"Answer how many elements the receiver contains."

	step < 0
		ifTrue: [start < stop
				ifTrue: [^ 0]
				ifFalse: [^ stop - start // step + 1]]
		ifFalse: [stop < start
				ifTrue: [^ 0]
				ifFalse: [^ stop - start // step + 1]]