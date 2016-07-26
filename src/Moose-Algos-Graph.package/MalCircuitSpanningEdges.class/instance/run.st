running
run

	self flag: #todo.	"for stability, we should sort edges by their label"
	self findDirectCircuits.
	self findIndirectCircuits
