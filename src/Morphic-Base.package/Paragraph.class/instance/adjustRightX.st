private
adjustRightX
	| shrink |
	shrink := container right - maxRightX.
	lines do: [:line | line paddingWidth: (line paddingWidth - shrink)].
	container := container withRight: maxRightX