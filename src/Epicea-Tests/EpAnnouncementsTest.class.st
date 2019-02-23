Class {
	#name : #EpAnnouncementsTest,
	#superclass : #EpMonitorIntegrationTest,
	#category : #'Epicea-Tests-Integration'
}

{ #category : #tests }
EpAnnouncementsTest >> assertMonitorAnnouncesUpdateWhen: aBlock [

	| wasAnnounced |
	wasAnnounced := false.

	monitor announcer
		when: EpMonitorStateUpdated
		do: [ :announcement | wasAnnounced := true ].
	
	aBlock
		ensure: [ monitor announcer unsubscribe: self ].

	self assert: wasAnnounced
	
]

{ #category : #tests }
EpAnnouncementsTest >> testMonitorAnnouncesUpdateWhenDisabled [

	self assertMonitorAnnouncesUpdateWhen: [ monitor disable ].

]

{ #category : #tests }
EpAnnouncementsTest >> testMonitorAnnouncesUpdateWhenEnabled [

	self assertMonitorAnnouncesUpdateWhen: [ monitor enable ]
]

{ #category : #tests }
EpAnnouncementsTest >> testMonitorAnnouncesUpdateWhenRedundantlyDisabled [

	monitor disable.
	self assertMonitorAnnouncesUpdateWhen: [ monitor disable ].

]

{ #category : #tests }
EpAnnouncementsTest >> testMonitorAnnouncesUpdateWhenRedundantlyEnabled [

	monitor enable.
	self assertMonitorAnnouncesUpdateWhen: [ monitor enable ].

]
