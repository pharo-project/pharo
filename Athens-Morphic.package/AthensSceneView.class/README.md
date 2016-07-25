i am simple morph for representing some scene.

The scene is any object which implements #renderOn: method,
or a block with single argument.
(an argument passed is an Athens canvas).

I implement a simple view panning and zooming with mouse drag and mouse-wheel (correspondigly).

Example1: open scene view, by passing a simple rendering block.

| view |
view  := AthensSceneView new.

view scene: [:canvas |
	canvas surface clear:  Color black.
	canvas setPaint: Color red.
	canvas drawShape: (0@0 corner:120@100)
].
view openInWindow.

Example2: open scene view on imported SVG file (note you need 'Athens-SVG' package loaded):

| view |
view  := AthensSceneView new.
view scene: (AthensSVGConverter fromFile: 'lion.svg').

view openInWindow.
