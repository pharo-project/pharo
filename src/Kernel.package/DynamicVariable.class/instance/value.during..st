accessing
value: anObject during: aBlock
	| activeProcess oldValue |
	activeProcess := Processor activeProcess.
	"oldValue will be nil if the variable has not yet been set"
	oldValue := activeProcess psValueAt: index.
	^ [ activeProcess
			psValueAt: index
			put: anObject.
		aBlock value ] ensure: [ 
			activeProcess
				psValueAt: index
				put: oldValue ]