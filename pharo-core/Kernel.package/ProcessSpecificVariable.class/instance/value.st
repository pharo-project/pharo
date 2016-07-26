accessing
value

	^ (Processor activeProcess psValueAt: index) ifNil: [ self default ]