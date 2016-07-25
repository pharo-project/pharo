setup
tearDown 
	
	Author fullName: previousAuthor.
	
	(createdClasses reject: [:c| c isObsolete])
		do: [:cls| 
				"(self packageClass organizer includesClass: cls)
					ifTrue: [self packageOrganizer unregisterClass: cls.]."
				cls removeFromSystem ].
	"createdPackages do:
		[:each | self packageOrganizer unregisterPackage: each]"
	
