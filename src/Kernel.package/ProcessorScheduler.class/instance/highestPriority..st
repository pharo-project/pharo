accessing
highestPriority: newHighestPriority
	"Change the number of priority levels currently available for use."

	| newProcessLists |
	(quiescentProcessLists size > newHighestPriority
		and: [self anyProcessesAbove: newHighestPriority])
			ifTrue: [self error: 'There are processes with priority higher than '
													,newHighestPriority printString].
	newProcessLists := Array new: newHighestPriority.
	1 to: ((quiescentProcessLists size) min: (newProcessLists size)) do: 
		[:priority | newProcessLists at: priority put: (quiescentProcessLists at: priority)].
	quiescentProcessLists size to: newProcessLists size do: 
		[:priority | newProcessLists at: priority put: ProcessList new].
	quiescentProcessLists := newProcessLists