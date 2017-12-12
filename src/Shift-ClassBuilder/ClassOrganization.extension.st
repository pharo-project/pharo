Extension { #name : #ClassOrganization }

{ #category : #'*Shift-ClassBuilder' }
ClassOrganization >> copyFrom: otherOrganization [
	comment := otherOrganization instVarNamed: #comment.
	commentStamp := otherOrganization commentStamp.
	(otherOrganization protocols
		collect: [ :e | 
			(Protocol name: e name)
				addAllMethodsFrom: e;
				yourself ]) do: [ :e | protocolOrganizer addProtocol: e ]
]
