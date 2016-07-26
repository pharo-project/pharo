I'm a MorphTreeModel enriched with a filter text field. When a pattern is entered in the textfield, then only appropriate root items are drawn in the tree. Very useful to filter a list of items. This model should be used everywhere a filter is added on top of a list as in Nautilus

Examples:
	"An example with auto-accept for the filter 
	(accepted as you type is the default)"
	| model morph |
	model := FilteredMorphTreeModel new.
	model rootItems: Morph allSubclasses.
	morph :=  model defaultMorphIn: World.
	morph extent: 300@500.
	morph openInWorld
	
	"Another example with a ghost string, and the user 
	has to accept the filter contents explicitly 
	with a return  or CMD-S in the text field"
	| model morph |
	model := FilteredMorphTreeModel new 
		ghostString: 'Enter a pattern'; 
		autoAccept: false; 
		yourself.
	model rootItems: Morph allSubclasses.
	morph :=  model defaultMorphIn: World.
	morph extent: 300@500.
	morph openInWorld
	
	"A dialog window with a filtered list"
	| window m |
	window := StandardWindow new model: self.
	window title: 'Test runner'.
	m := FilteredMorphTreeModel new
		wrapBlockOrSelector: #selector;
		rootItems: Morph methods.
	window addMorph: (m defaultMorphIn: window) fullFrame: LayoutFrame identity.
	window openInWorld

Internal Representation and Key Implementation Points.
The initial root item list is kept locally.
See #rootItems:

Instance Variables
	ghostString:		<String>
	autoAccept:		<Boolean>
	initialItems:		<Collection>
	patternModel:		<RubScrolledTextModel>