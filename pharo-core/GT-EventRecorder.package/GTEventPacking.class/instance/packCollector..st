private
packCollector: aGTEventCollector
	^ self serialize: (self newAnnouncementFor: aGTEventCollector)