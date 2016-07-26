serializing
serializeAnnouncementWithAllEventsUsing: aGTEventPacking
	"self serializeAnnouncementWithAllEventsUsing: GTEventPacking default"
	| collector data |
	collector := GTSpotterEventCollector new.
	self instancesForAllEvents do: [ :eachEvent | collector add: eachEvent ].
	data := (aGTEventPacking pack: collector) data.
	[ data asByteArray == data ] assert.
	^ data