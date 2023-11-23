"
I announce when the profiling results tree changes root
"
Class {
	#name : 'ProfilerResultsTreeRootChangedAnnouncement',
	#superclass : 'Announcement',
	#instVars : [
		'item'
	],
	#category : 'ProfilerUI-Announcements',
	#package : 'ProfilerUI',
	#tag : 'Announcements'
}

{ #category : 'accessing' }
ProfilerResultsTreeRootChangedAnnouncement >> item [

	^ item
]

{ #category : 'accessing' }
ProfilerResultsTreeRootChangedAnnouncement >> item: anObject [

	item := anObject
]
