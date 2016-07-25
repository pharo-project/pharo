*Morphic-Base
indentationIfBlank: aBlock 
	"Answer the number of leading tabs in the receiver.  If there are
	 no visible characters, pass the number of tabs to aBlock and return its value."
	| reader leadingTabs lastSeparator cr tab ch |
	cr := Character cr.
	tab := Character tab.
	reader := self readStream.
	leadingTabs := 0.
	[ reader atEnd not and: [ (ch := reader next) = tab ] ] whileTrue: [ leadingTabs := leadingTabs + 1 ].
	lastSeparator := leadingTabs + 1.
	[ reader atEnd not and: [ ch isSeparator and: [ ch ~= cr ] ] ] whileTrue: 
		[ lastSeparator := lastSeparator + 1.
		ch := reader next ].
	(lastSeparator = self size or: [ ch = cr ]) ifTrue: [ ^ aBlock value: leadingTabs ].
	^ leadingTabs