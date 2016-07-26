system annoucements
methodRemoved: anAnnouncement

	| classes |
	classes := rbEnvironment packages flatCollect: [ :package | package classes].
	(classes anySatisfy: [ :cl | anAnnouncement methodClass = cl ]) 
				ifFalse: [ ^ self ].
	cache itemRemoved: anAnnouncement method