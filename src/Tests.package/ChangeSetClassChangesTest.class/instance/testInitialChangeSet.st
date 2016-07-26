testing
testInitialChangeSet
	"Run this to assure the initial changeset is named. Checks bug found in 3.9 7052."
	"self new testInitialChangeSet"
	"self run:  #testInitialChangeSet"

	self deny: (ChangeSet current printString = 'a ChangeSet named <no name -- garbage?>') .
	
^true