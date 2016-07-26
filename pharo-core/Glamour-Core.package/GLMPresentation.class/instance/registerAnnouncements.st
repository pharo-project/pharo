updating
registerAnnouncements
	"this method is called from the rendering code"
	self flag: 'The announcements should potentially be interested in any ports, not just in entity'.
	updateActions do: [:each | each registerInPresentation ].