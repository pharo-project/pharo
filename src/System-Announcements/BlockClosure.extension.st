Extension { #name : #BlockClosure }

{ #category : #'*System-Announcements' }
BlockClosure >> valueWithoutNotifications [ 
	^SystemAnnouncer uniqueInstance suspendAllWhile: self
]
