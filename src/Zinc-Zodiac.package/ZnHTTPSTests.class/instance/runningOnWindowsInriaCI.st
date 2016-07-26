private
runningOnWindowsInriaCI
	| platform ci |
	self runningOnWindows
		ifFalse: [ ^ false ].
	^ [ 
	platform := OSPlatform current.
	ci := platform environment at: 'CI' ifAbsent: [ '' ].
	ci = 'ci.inria.fr' ]
		on: MessageNotUnderstood
		do: [ false ]