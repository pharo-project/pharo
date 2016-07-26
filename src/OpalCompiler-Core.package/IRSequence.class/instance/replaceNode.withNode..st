replacing
replaceNode: aNode withNode: anotherNode 

	| index |
	index := sequence indexOf: aNode ifAbsent: [ self error ].
	sequence at: index put: (anotherNode sequence: self).