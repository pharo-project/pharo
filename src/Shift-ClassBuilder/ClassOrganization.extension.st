Extension { #name : #ClassOrganization }

{ #category : #'*Shift-ClassBuilder' }
ClassOrganization >> copyFrom: otherOrganization [
	commentRemoteString := otherOrganization commentRemoteString.
	commentStamp := otherOrganization commentStamp.
	otherOrganization protocols do: [ :p | p methodSelectors do: [ :m | protocolOrganizer classify: m inProtocolNamed: p name ] ]
]
