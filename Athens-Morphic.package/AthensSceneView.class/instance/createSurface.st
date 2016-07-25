session management
createSurface
	| extent |
	extent := self extent asIntegerPoint.
	surface := (scene respondsTo: #createSurface:) 
		ifTrue: [ scene createSurface: extent ] 
		ifFalse: [ AthensCairoSurface extent: extent].
