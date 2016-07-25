initialization
basicOrganizationChange
	^ organizationChange ifNil: [ organizationChange := IcePackageOrganizationChange new. ]
