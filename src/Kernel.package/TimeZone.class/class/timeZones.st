accessing
timeZones

	^ {
		self offset:  0 hours name: 'Universal Time'              abbreviation: 'UTC'.
		self offset:  0 hours name: 'Greenwich Mean Time'         abbreviation: 'GMT'.
		self offset:  1 hours name: 'British Summer Time'         abbreviation: 'BST'.
		self offset:  2 hours name: 'South African Standard Time' abbreviation: 'SAST'.
		self offset: -8 hours name: 'Pacific Standard Time'       abbreviation: 'PST'.
		self offset: -7 hours name: 'Pacific Daylight Time'       abbreviation: 'PDT'.
		self offset: -7 hours name: 'Mountain Standard Time'      abbreviation: 'MST'.
		self offset: -6 hours name: 'Mountain Daylight Time'      abbreviation: 'MDT'.
		self offset: -6 hours name: 'Central Standard Time'       abbreviation: 'CST'.
		self offset: -5 hours name: 'Central Daylight Time'       abbreviation: 'CDT'.
		self offset: -5 hours name: 'Eastern Standard Time'       abbreviation: 'EST'.
		self offset: -4 hours name: 'Eastern Daylight Time'       abbreviation: 'EDT'.
	}

