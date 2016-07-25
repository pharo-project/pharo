fonts and font indexes
flushFonts
	"Clean out the fonts, an aid when snapshotting claims too many are 
	holding onto Display."

	(self confirm: 
'flushFonts is very dangerous.
Are you foolish or clever enough to proceed?')
		ifTrue: [1 to: fontArray size do: [:index | fontArray at: index put: nil]]
		ifFalse: [Transcript cr; show: 'flushFonts cancelled']

	"TextStyle default flushFonts"