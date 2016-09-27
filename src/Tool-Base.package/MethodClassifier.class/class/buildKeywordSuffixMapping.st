private
buildKeywordSuffixMapping
	keywordSuffixMapping := Dictionary new
		at: 'Add:' put: 'adding';
		at: 'AddAll:' put: 'adding';
		at: 'Associations' put: 'accessing';
		at: 'At:' put: 'accessing';
		at: 'Collect:' put: 'enumerating';
		at: 'Detect:' put: 'enumerating';
		at: 'Do:' put: 'enumerating';
		at: 'Includes:' put: 'testing';
		at: 'Keys' put: 'accessing';
		at: 'Reject:' put: 'enumerating';
		at: 'Remove:' put: 'removing';
		at: 'RemoveAll' put: 'removing';
		at: 'RemoveKey:' put: 'removing';
		at: 'Select:' put: 'enumerating';
		at: 'Values' put: 'accessing';
		yourself