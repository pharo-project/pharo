private
portEvent: aPortEvent

	"we unregister all presentations from announcements because they will get replaced anyway"
	aPortEvent portName = #entity ifTrue: [ self unregisterFromAllAnnouncements ].
	self browser innerPortEvent: aPortEvent.
	self presentations outerPortEvent: aPortEvent