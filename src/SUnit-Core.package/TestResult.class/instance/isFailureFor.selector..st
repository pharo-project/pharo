querying
isFailureFor: class selector: selector
	^ self failures anySatisfy: [:testCase | testCase class == class and: [testCase selector == selector]]