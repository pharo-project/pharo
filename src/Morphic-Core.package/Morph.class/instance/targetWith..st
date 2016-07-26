meta-actions
targetWith: evt
	"Some other morph become target of the receiver"
	|  newTarget |
	newTarget := UIManager default
				chooseFrom: (self potentialTargets
						collect: [:t | t class name asString])
				values: self potentialTargets
				title: (self externalName, ' targets...' translated).
	newTarget ifNil:[^self].
	self target: newTarget.