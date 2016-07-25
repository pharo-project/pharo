computing types
computeTypes
	variableMessages
		keysAndValuesDo: [:key :value | variableTypes at: key put: (self findTypeFor: value)]