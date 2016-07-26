system changes
noMoreAnnounceWhile: aBlock
	"unplug the announcer during aBlock"
	| oldAnnouncing |
	[oldAnnouncing := announcing. 
	announcing := false.
	aBlock value]
		ensure: [announcing := oldAnnouncing]