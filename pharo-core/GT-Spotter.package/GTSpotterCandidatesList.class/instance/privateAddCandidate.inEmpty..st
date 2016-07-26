private
privateAddCandidate: aCandidateLink inEmpty: aProcessorLink
	| head current |
		
	current := aProcessorLink.	"not nil"
	head := nil.						"nil"
	[ head isNil and: [ current isNotNil ]] whileTrue: [	
		current := current nextLink.	"next category"
		"head of next category if not nil break loop"
		current ifNotNil: [ head := current headLink ] ifNil: [ nil ] ].
	
	"means all next categories are empty, just add to the end"
	current
		ifNil: [ self candidates addLast: aCandidateLink ]
		ifNotNil: [ self candidates add: aCandidateLink beforeLink: head ].
		aProcessorLink
			headLink: aCandidateLink;
			tailLink: aCandidateLink.
			
		aProcessorLink incrementSize.
		
	^ aCandidateLink