querying
isPassedFor: class selector: selector
	^ self passed anySatisfy: [:testCase | testCase class == class and: [testCase selector == selector]]