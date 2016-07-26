scrolling
hIsScrollbarNeeded
"Return whether the horz scrollbar is needed"

	"Don't show it if we were told not to."
	(self valueOfProperty: #noHScrollBarPlease ifAbsent: [false]) ifTrue: [^false].

	"Always show it if we were told to"
	(self valueOfProperty: #hScrollBarAlways ifAbsent: [false]) ifTrue: [^true].

	^self hIsScrollable
