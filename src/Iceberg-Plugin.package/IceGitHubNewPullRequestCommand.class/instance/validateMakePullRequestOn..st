private
validateMakePullRequestOn: aRepository
	| status |
	
	status := OrderedCollection new: 2.
	aRepository isModified 
		ifTrue: [ status add: 'Uncommited changes' ].
	(aRepository outgoingCommitsTo: (aRepository pushRemote)) 
		ifNotEmpty: [ :commits | '{1} not published' format: { commits size } ].
	status ifEmpty: [ ^ true ].
	
	^ UIManager default 
		confirm: (('{1} has ongoing modifications ({2}). 
Do you want to continue anyway?') format: { aRepository name. status asCommaString })