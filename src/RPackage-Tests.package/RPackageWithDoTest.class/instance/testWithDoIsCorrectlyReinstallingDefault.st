tests
testWithDoIsCorrectlyReinstallingDefault
	"self debug: #testWithDoIsCorrectlyReinstallingDefault"
	
	| current empty |

	current := self packageOrganizerClass default.  
	empty := self packageOrganizerClass basicNew initialize.
	empty debuggingName: 'empty from PackageWithDoTest'.
	self packageClass 
		withOrganizer: empty
		do: [ 
			self assert: (self announcer hasSubscriber: empty).
			self deny: (self announcer hasSubscriber: current)].
	self assert: (self announcer hasSubscriber: current).
	self deny: (self announcer hasSubscriber: empty)