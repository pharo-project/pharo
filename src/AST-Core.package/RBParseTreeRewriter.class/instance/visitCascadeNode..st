visitor-double dispatching
visitCascadeNode: aCascadeNode 
	| newMessages notFound |
	newMessages := OrderedCollection new: aCascadeNode messages size.
	notFound := OrderedCollection new: aCascadeNode messages size.
	aCascadeNode messages do: 
			[:each | 
			| newNode |
			newNode := self performSearches: searches on: each.
			newNode isNil 
				ifTrue: 
					[newNode := each.
					notFound add: newNode].
			newNode isMessage 
				ifTrue: [newMessages add: newNode]
				ifFalse: 
					[newNode isCascade 
						ifTrue: [newMessages addAll: newNode messages]
						ifFalse: 
							[Transcript
								show: 'Cannot replace message node inside of cascaded node with non-message node.';
								cr.
							newMessages add: each]]].
	notFound size = aCascadeNode messages size 
		ifTrue: 
			[| receiver |
			receiver := self visitNode: aCascadeNode messages first receiver.
			newMessages do: [:each | each receiver: receiver]].
	notFound 
		do: [:each | each arguments: (each arguments collect: [:arg | self visitNode: arg])].
	aCascadeNode messages: newMessages