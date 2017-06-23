*Iceberg-UI
updateDiff: aDiffModel
	aDiffModel
		contextClass: nil;
		leftText: (self organizationDescription: self newOrganization);
		rightText: (self organizationDescription: self oldOrganization).
