tests
testWithDoIsCorrectlyReinstallingDefaultEvenIfHalt
	"self debug: #testWithDoIsCorrectlyReinstallingDefaultEvenIfHalt"
	
	| current empty |
	current := self packageOrganizerClass default.  
	empty := self packageOrganizerClass basicNew initialize.
	[ self packageClass 
		withOrganizer: empty
		do: [ 		
			self assert: (self announcer hasSubscriber: empty).
			self deny: (self announcer hasSubscriber: current).
			self error ] ]
		on: Error do: [:ex|].
	
	self assert: (self announcer hasSubscriber: current).
	self deny: (self announcer hasSubscriber: empty)
		
		


