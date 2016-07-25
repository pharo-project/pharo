*Morphic-Widgets-Windows
nextWindow
	"Answer the next window to navigate to."

	|sys|
	sys := self systemWindows.
	sys ifEmpty: [^nil].
	^sys after: self currentWindow ifAbsent: [sys first]