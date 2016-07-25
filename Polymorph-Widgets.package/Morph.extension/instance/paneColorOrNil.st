*Polymorph-Widgets
paneColorOrNil
	"Answer the window's pane color or nil otherwise."

	^self valueOfProperty: #paneColor ifAbsent: [
		(self owner ifNil: [^nil]) paneColorOrNil]