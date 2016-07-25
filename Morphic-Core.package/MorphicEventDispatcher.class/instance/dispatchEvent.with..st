dispatching
dispatchEvent: anEvent with: aMorph
	"Dispatch the given event for a morph that has chosen the receiver to dispatch its events. "
	morph := aMorph.
	[ ^ anEvent sentTo: self ]
	ensure: [ morph := nil ]