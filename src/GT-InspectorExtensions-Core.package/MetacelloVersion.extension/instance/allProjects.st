*GT-InspectorExtensions-Core
allProjects
	| all |
	all := OrderedCollection new.
	self allProjectsDo: [ :each |
		all add: each ].
	^ all