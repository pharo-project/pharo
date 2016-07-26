announcement handling
actOn: anAnnouncement
	| allowedToTrigger |
	allowedToTrigger := self condition glamourValue: (	
							anAnnouncement asGlamorousMultiValue, 
							self presentation entity asGlamorousMultiValue).
	allowedToTrigger ifTrue: [ self presentation update ]