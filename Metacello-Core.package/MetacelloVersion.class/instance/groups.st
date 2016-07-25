querying
groups
	"Answers the list of groups associated with this version"
	
	| groups |
	groups := OrderedCollection new.
	self spec projectDo: [:ignored | ] packageDo: [:ignored | ] groupDo: [:grp | groups add: grp ].
	^groups