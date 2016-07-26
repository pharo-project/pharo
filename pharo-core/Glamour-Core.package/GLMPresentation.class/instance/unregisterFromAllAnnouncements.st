updating
unregisterFromAllAnnouncements
	"this method should be called every time the presentation is not needed"
	updateActions do: [ :each | each unregisterFromAllAnnouncements ]