private
noteRecentClipping: text
	"Keep most recent clippings in a queue for pasteRecent (paste... command)"
	text isEmpty ifTrue: [^ self].
	text size > 50000 ifTrue: [^ self].
	(recent includes: text) ifTrue: [^ self].
	recent addFirst: text.
	[recent size > 5] whileTrue: [recent removeLast].
