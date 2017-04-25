building
countRegisters
	fields do: [ :field |
		field registerClass == #integer ifTrue: [ integerRegisterCount := integerRegisterCount + 1 ].
		field registerClass == #float ifTrue: [ floatRegisterCount := floatRegisterCount + 1 ].
	]