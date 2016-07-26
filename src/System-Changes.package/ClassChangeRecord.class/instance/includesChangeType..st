all changes
includesChangeType: changeType

	changeType == #new ifTrue: [^ changeTypes includes: #add].  "Backwd compat"
	^ changeTypes includes: changeType