private
collectorForAssignment
	^ collectorForAssignment ifNil: [collectorForAssignment := self createCollector: #assignmentCommand].
