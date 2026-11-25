Extension { #name : 'Form' }

{ #category : '*Morphic-Desktop' }
Form class >> serviceOpenImageInWindow [
	"Answer a service for opening a graphic in a window"
	<formService>
	^ FileServiceEntry
		provider: self
		label: 'Open graphic in a window'
		selector: #openImageInWindow:
		description: 'Open a graphic file in a window'
		buttonLabel: 'Open'
]
