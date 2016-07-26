OBSOLETE
jobEnded: anAnnouncement
	<deprecated: 'a call of this method means, there are still old job announcements in this image based on this instance. we do nothing here except wiping all previous announcers. we reset/fix this instance to use the new announcers'>
	[ self stopThoroughly; reset; start; loadStopped: anAnnouncement ] on: Error do: [ " ignore " ]