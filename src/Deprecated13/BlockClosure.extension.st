Extension { #name : 'BlockClosure' }

{ #category : '*Deprecated13' }
BlockClosure >> valueWithoutNotifications [

	self deprecated: 'System announcement will be splitted between two announcers. This code should be replaced by either of those:
	
	self class codeChangeAnnouncer suspendAllWhile: [ ... ] 
	
	or
	
	self class codeSupportAnnouncer suspendAllWhile: [ ... ] 
	
	This will depend if you want to suspend announcements about the code changes in Pharo such as MethodAdded or ClassRemoved or announcements about Pharo tools such as BreakpointAdded
	'.
	^ SystemAnnouncer uniqueInstance suspendAllWhile: self
]
