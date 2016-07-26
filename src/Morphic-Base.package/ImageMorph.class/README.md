ImageMorph is a morph that displays a picture (Form). My extent is determined by the extent of my form.

Use #image: to set my picture.

Structure:
 instance var		Type 		Description
 image				Form		The Form to use when drawing

Code examples:
	ImageMorph new openInWorld; grabFromScreen

	(Form fromFileNamed: 'myGraphicsFileName') asMorph openInWorld

Relationship to SketchMorph: ImageMorph should be favored over SketchMorph, a parallel, legacy class -- see the Swiki FAQ for details ( http://minnow.cc.gatech.edu/squeak/1372 ). 
