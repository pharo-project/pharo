private
applySharedVariableOrPoolChange: classModification
	"This method updates both shared variables and shared pools since they use the same mechanism"
	result := self applyAndUpdateSharedVariableOrSharedPool: classModification.
	classModification allPropagationsDo: [ :propagation | 
		self applyAndUpdateSharedVariableOrSharedPool: propagation ]