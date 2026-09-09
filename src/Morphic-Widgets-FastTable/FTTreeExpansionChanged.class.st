"
I am an announcement raised when a tree node is expanded or collapsed.

I carry the item data and whether it was expanded (true) or collapsed (false).
"
Class {
	#name : 'FTTreeExpansionChanged',
	#superclass : 'FTAnnouncement',
	#instVars : [
		'item',
		'expanded'
	],
	#category : 'Morphic-Widgets-FastTable-Announcement',
	#package : 'Morphic-Widgets-FastTable',
	#tag : 'Announcement'
}

{ #category : 'instance creation' }
FTTreeExpansionChanged class >> item: anItem expanded: aBoolean [
	
	^ self new
		  item: anItem;
		  expanded: aBoolean;
		  yourself
]

{ #category : 'accessing' }
FTTreeExpansionChanged >> expanded [

	^ expanded
]

{ #category : 'accessing' }
FTTreeExpansionChanged >> expanded: aBoolean [

	expanded := aBoolean
]

{ #category : 'accessing' }
FTTreeExpansionChanged >> item [

	^ item
]

{ #category : 'accessing' }
FTTreeExpansionChanged >> item: anObject [

	item := anObject
]
