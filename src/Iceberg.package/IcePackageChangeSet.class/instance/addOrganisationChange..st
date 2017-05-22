private dispatch
addOrganisationChange: change
	change isAddition ifTrue: [ ^ self newOrganization: change definition categories ].
	change isRemoval ifTrue: [ ^ self oldOrganization: change definition categories ].
	self error: 'Can not handle definition'.