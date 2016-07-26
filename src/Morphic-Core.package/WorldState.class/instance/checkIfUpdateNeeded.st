update cycle
checkIfUpdateNeeded

	damageRecorder updateIsNeeded ifTrue: [^true].
	hands do: [:h | (h hasChanged and: [h needsToBeDrawn]) ifTrue: [^true]].
	^false  "display is already up-to-date"
