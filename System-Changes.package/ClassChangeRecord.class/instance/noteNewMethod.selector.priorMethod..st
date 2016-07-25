method changes
noteNewMethod: newMethod selector: selector priorMethod: methodOrNil
	| methodChange |
	methodChange := self findOrMakeMethodChangeAt: selector priorMethod: methodOrNil.
	methodOrNil 
		ifNil: [ methodChange noteChangeType: #add ]
		ifNotNil: [ methodChange noteChangeType: #change ].