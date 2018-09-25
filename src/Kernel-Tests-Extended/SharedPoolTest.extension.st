Extension { #name : #SharedPoolTest }

{ #category : #'*Kernel-Tests-Extended' }
SharedPoolTest >> testPoolUsers [
	| result |
	result := ChronologyConstants poolUsers.
	self assert: result asSet equals: {Date. DateAndTime. Duration. Month. Time. TimeZone. Week. LocalTimeZone . AbstractTimeZone } asSet.
		 
]
