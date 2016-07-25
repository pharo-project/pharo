accessing
optionDash
	"VMs release in Mai 2013 use -- instead of - for VM command line options"
	^ (Smalltalk vm interpreterSourceDate > '2013-05-17' asDate)
		ifTrue: [ '--' ]
		ifFalse: [ '-' ]