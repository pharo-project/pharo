A Morph (from the Greek "shape" or "form") is an interactive graphical object. General information on the Morphic system can be found at http://minnow.cc.gatech.edu/squeak/30. 

Morphs exist in a tree, rooted at a World (generally a PasteUpMorph). The morphs owned by a morph are its submorphs. Morphs are drawn recursively; if a Morph has no owner it never gets drawn. To hide a Morph and its submorphs, set its #visible property to false using the #visible: method. 

The World (screen) coordinate system is used for most coordinates, but can be changed if there is a TransformMorph somewhere in the owner chain. 

My instance variables have accessor methods (e.g., #bounds, #bounds:). Most users should use the accessor methods instead of using the instance variables directly.

Structure:
instance var 	Type 			Description 
bounds 			Rectangle 		A Rectangle indicating my position and a size that will enclose 									me. 
owner 			Morph		 	My parent Morph, or nil for the top-level Morph, which is a
 				or nil			world, typically a PasteUpMorph.
submorphs 		Array 			My child Morphs. 
fullBounds 		Rectangle 		A Rectangle minimally enclosing me and my submorphs. 
color 			Color 			My primary color. Subclasses can use this in different ways. 
extension 		MorphExtension Allows extra properties to be stored without adding a
				or nil  				storage burden to all morphs. 

By default, Morphs do not position their submorphs. Morphs may position their submorphs directly or use a LayoutPolicy to automatically control their submorph positioning.

Although Morph has some support for BorderStyle, most users should use BorderedMorph if they want borders.