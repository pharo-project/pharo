announcement
mcPackageModified: anAnnouncement
	| package |

	package := anAnnouncement package koPackage.
	
	package patch isEmpty
		ifTrue: [ self stagingArea removePackage: package ]
		ifFalse: [ self stagingArea addPackage: package ].
	
	view ifNil: [ ^ self ].
	
	package patch isEmpty
		ifTrue: [ view removeRoot: package ]
		ifFalse: [ view addRoot: package ]