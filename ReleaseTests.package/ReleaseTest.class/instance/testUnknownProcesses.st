testing
testUnknownProcesses
	"Make sure that there are no other processes running except the known processes"
	
	| unknownProcesses |
	unknownProcesses := self actualProcesses difference: self knownProcesses.
	self 
		assert: unknownProcesses size <=2
		description: (String streamContents: [ :out |
			out << 'Found unknown processes: '.
			unknownProcesses 
				do: [ :each | out print: each ] 
				separatedBy: [ out << ', ' ] ])