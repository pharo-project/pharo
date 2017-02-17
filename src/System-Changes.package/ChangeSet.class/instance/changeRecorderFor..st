private
changeRecorderFor: class
	| cname |
	cname := class printString.
	^ changeRecords at: cname ifAbsent: [ ^ changeRecords at: cname put: (ClassChangeRecord new initFor: cname) ]