reading
systemOrganizationFromRecords: changeRecords
	| categories |
	categories := changeRecords
					select: [:ea | 'SystemOrganization*' match: ea string]
					thenCollect: [:ea | (self categoryFromDoIt: ea string)].
	^ categories isEmpty ifFalse: [MCOrganizationDefinition categories: categories asArray]