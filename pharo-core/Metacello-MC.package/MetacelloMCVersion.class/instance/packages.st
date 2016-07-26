querying
packages
	"Answers the list of packages associated with this version"

	| packages |
	packages := OrderedCollection new.
	self spec projectDo: [:ignored | ] packageDo: [:pkg | packages add: pkg ] groupDo: [:ignored | ].
	^packages