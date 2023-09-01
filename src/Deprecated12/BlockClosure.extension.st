Extension { #name : #BlockClosure }

{ #category : #'*Deprecated12' }
BlockClosure >> valueWithoutNotifications [

	self deprecated: 'This method will be removed from future version of Pharo since the meta model will not rely on the singleton anymore.'.
	^ SystemAnnouncer uniqueInstance suspendAllWhile: self
]
