Extension { #name : #ToolRegistry }

{ #category : #'*Deprecated12' }
ToolRegistry >> whenToolRegistered: aBlock [

	| subscriberForDeprecation |
	aBlock receiver ifNil: [
		self error:
			'You must specify a subscriber object for this subscription. Please use #whenToolRegisteredDo:for: method.' ].

	subscriberForDeprecation := thisContext sender receiver
	                            = aBlock receiver
		                            ifTrue: [ 'self' ]
		                            ifFalse: [ '`@arg1 receiver' ].
	self
		deprecated:
			'Since there are some block closures (Clean and Constant) without a receiver, the API of announcements was changed to send the subscriber explicitly. 
			We are deprecating this method because it was asking for the receiver of the block to use it as the subscriber.'
		transformWith: '`@receiver whenToolRegistered: `@arg1'
			-> ('`@receiver whenToolRegisteredDo: `@arg1 for: '
				 , subscriberForDeprecation).

	self announcer weak
		when: ToolRegistryToolRegistered
		do: aBlock
		for: aBlock receiver
]

{ #category : #'*Deprecated12' }
ToolRegistry >> whenToolUnregistered: aBlock [

	| subscriberForDeprecation |
	aBlock receiver ifNil: [
		self error:
			'You must specify a subscriber object for this subscription. Please use #whenToolUnregisteredDo:for: method.' ].

	subscriberForDeprecation := thisContext sender receiver
	                            = aBlock receiver
		                            ifTrue: [ 'self' ]
		                            ifFalse: [ '`@arg1 receiver' ].
	self
		deprecated:
			'Since there are some block closures (Clean and Constant) without a receiver, the API of announcements was changed to send the subscriber explicitly. 
			We are deprecating this method because it was asking for the receiver of the block to use it as the subscriber.'
		transformWith: '`@receiver whenToolUnregistered: `@arg1'
			-> ('`@receiver whenToolUnregisteredDo: `@arg1 for: '
				 , subscriberForDeprecation).

	self announcer weak
		when: ToolRegistryToolUnregistered
		do: aBlock
		for: aBlock receiver
]
