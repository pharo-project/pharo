initialization
analyseChanges
	| newChanges |
	newChanges := self parent changes
		select: [ :change | change icePackage = self package ].
	
	changes := OrderedCollection new.
	newChanges do: [ :change | change definition isOrganizationDefinition
		ifTrue: [ 
			change isAddition 
				ifTrue: [ self newOrganization: change definition categories ]
				ifFalse: [ 
					change isRemoval 
						ifTrue: [ self oldOrganization: change definition categories ]
						ifFalse: [ self error: 'Can not handle definition' ]]]
		ifFalse: [ changes add: change ]].
