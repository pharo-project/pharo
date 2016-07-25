accessing
valueOrNil
	"a faster version, which doesn't using ifAbsent: to avoid using block closure"
	^ Processor activeProcess psValueAt: index 