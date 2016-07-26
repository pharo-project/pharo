event handling
verticalScroll: anAnnouncement
	"Called from src when scrolled by keyboard etc.."

	self srcMorph scrollPane vScrollbarValueWithoutAnnouncement: anAnnouncement scrollValue.
	self dstMorph scrollPane vScrollbarValueWithoutAnnouncement: anAnnouncement scrollValue.
	self scrollbarMorph value: anAnnouncement scrollValue.
	self updateJoinOffsets