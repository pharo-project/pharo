*Morphic-Widgets-Windows
previousWindow
	"Answer the previous window to navigate to."

	|sys|
	sys := self systemWindows.
	sys ifEmpty: [^nil].
	^sys before: self currentWindow ifAbsent: [sys last]