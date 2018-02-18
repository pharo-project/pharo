"
I am a subclass which work when VM does not supports finalization lists.
I am about 3 times slower when it comes to finalizing items
"
Class {
	#name : #LegacyWeakSubscription,
	#superclass : #WeakAnnouncementSubscription,
	#type : #weak,
	#category : #'Announcements-Core'
}
