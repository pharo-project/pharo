testing
includesClass: aClass 
	(environment includesClass: aClass) ifFalse: [^true].
	^false