menus behavior
restrictedBrowseTraitUsers: users

	| newEnvironment |
	users ifEmpty: [ ^ self ].
	newEnvironment := self browsedEnvironment forClasses: users.
	self model class 
		openOnClass: users anyOne 
		inEnvironment: newEnvironment 