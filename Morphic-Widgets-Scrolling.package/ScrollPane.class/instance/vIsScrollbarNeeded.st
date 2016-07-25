scrolling
vIsScrollbarNeeded
"Return whether the verticle scrollbar is needed"
	
	"Don't show it if we were told not to."
	(self valueOfProperty: #noVScrollBarPlease ifAbsent: [false]) ifTrue: [^false].

	"Always show it if we were told to"
	(self valueOfProperty: #vScrollBarAlways ifAbsent: [false]) ifTrue: [^true].
	
	^self vIsScrollable
