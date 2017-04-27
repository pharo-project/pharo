filtering
accepts: anOmEntry

	| finalReference log |
	log := logBrowserModel log.
	finalReference := log referenceTo: anOmEntry.
	anOmEntry content isCodeChange ifFalse: [ ^ false ].

	self entriesFromHead do: [:each |
		(log referenceTo: each) = finalReference ifTrue: [ ^ true ].
		(each content isCodeChange and: [
			each content doesOverride: anOmEntry content ]) ifTrue: [ ^ false ].
	].

	^ false "shouldn't happen"