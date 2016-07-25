private
runningOnWindows
	"Pharo 3+"
	[ ^ OSPlatform current isWindows ] on: MessageNotUnderstood do: [ ].
	"Pharo 2-"
	[ ^ OSPlatform isWin32 ] on: MessageNotUnderstood do: [ ].
	"Give up"
	^ false