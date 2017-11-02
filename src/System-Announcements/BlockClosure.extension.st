Extension { #name : #BlockClosure }

{ #category : #'*system-announcements' }
BlockClosure >> valueWithoutNotifications [ 
	^SystemAnnouncer uniqueInstance suspendAllWhile: self
]
