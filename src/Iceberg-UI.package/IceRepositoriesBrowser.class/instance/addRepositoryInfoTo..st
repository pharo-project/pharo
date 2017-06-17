building
addRepositoryInfoTo: composite
	composite fastTable
		title: 'General';
		display: #detailedInfo;
		column: 'Name' evaluated: #key width: 80;
		column: 'Value' evaluated: #value