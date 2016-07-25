querying
isErrorFor: class selector: selector
	^ self errors anySatisfy: [:testCase | testCase class == class and: [testCase selector == selector]]