Extension { #name : #Announcer }

{ #category : #'*Deprecated12' }
Announcer >> when: anAnnouncementClass do: aValuable [

	| subscriberForDeprecation |
	aValuable receiver ifNil: [
		self error:
			'You must specify a subscriber object for this subscription. Please use #when:do:for: method.' ].

	subscriberForDeprecation := thisContext sender receiver = aValuable receiver
		                            ifTrue: [ 'self' ]
		                            ifFalse: [ '`@arg2 receiver' ].
	self
		deprecated:
			'Since there are some block closures (Clean and Constant) without a receiver, the API of announcements was changed to send the subscriber explicitly. 
			We are deprecating this method because it was asking for the receiver of the block to use it as the subscriber.'
		transformWith: '`@receiver when: `@arg1 do: `@arg2'
			->
			('`@receiver when: `@arg1 do: `@arg2 for: '
			 , subscriberForDeprecation).

	self when: anAnnouncementClass do: aValuable for: aValuable receiver
]
