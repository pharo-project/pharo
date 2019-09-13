Extension { #name : #WeakSubscriptionBuilder }

{ #category : #'*Deprecated80' }
WeakSubscriptionBuilder >> subscribe: anAnnouncementClass do: aValuable [
	"Do not use this message on weak announcer because it does not work. The block will hold strongly the receiver and more. We need ephemerons for that"

	self deprecated: 'Please use #when:send:to: instead. And do not use when:do: on weak announcer'.

	^ self when: anAnnouncementClass do: aValuable
]
