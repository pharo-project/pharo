instruction decoding
doDup

	stack last == CascadeFlag
		ifFalse:
			["Save position and mark cascade"
			stack addLast: statements size.
			stack addLast: CascadeFlag].
	stack addLast: CascadeFlag