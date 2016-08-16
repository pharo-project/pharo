private
buildKeywordSuffixMapping
	(keywordSuffixMapping := Dictionary new)
		at: 'At:' put: 'accessing';
		at: 'Keys' put: 'accessing';
		at: 'Values' put: 'accessing';
		at: 'Associations' put: 'accessing';
		at: 'Add:' put: 'adding';
		at: 'AddAll:' put: 'adding';
		at: 'Remove:' put: 'removing';
		at: 'RemoveKey:' put: 'removing';
		at: 'RemoveAll' put: 'removing';
		at: 'Includes:' put: 'testing';
		at: 'Collect:' put: 'enumerating';
		at: 'Detect:' put: 'enumerating';
		at: 'Do:' put: 'enumerating';
		at: 'Reject:' put: 'enumerating';
		at: 'Select:' put: 'enumerating'