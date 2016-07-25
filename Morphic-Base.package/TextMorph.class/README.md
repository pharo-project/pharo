TextMorphs support display of text with emphasis.  They also support reasonable text-editing capabilities, as well as embedded hot links, and the ability to embed submorphs in the text.

Late in life, TextMorph was made a subclass of BorderedMorph to provide border and background color if desired.  In order to keep things compatible, protocols have been redirected so that color (preferably textColor) relates to the text, and backgroundColor relates to the inner fill color.

Text display is clipped to the innerBounds of the rectangle, and text composition is normally performed within a rectangle which is innerBounds inset by the margins parameter.

If text has been embedded in another object, one can elect to fill the owner's shape, in which case the text will be laid out in the shape of the owner's shadow image (including any submorphs other than the text).  One can also elect to have the text avoid occlusions, in which case it will avoid the bounds of any sibling morphs that appear in front of it.  It may be necessary to update bounds in order for the text runaround to notice the presence of a new occluding shape.

The optional autoFitContents property enables the following feature:  if the text contents changes, then the bounds of the morph will be adjusted to fit the minimum rectangle that encloses the text (plus any margins specified).  Similarly, any attempt to change the size of the morph will be resisted if this parameter is set.  Except...

If the wrapFlag parameter is true, then text will be wrapped at word boundaries based on the composition width (innerBounds insetBy: margins) width.  Thus an attempt to resize the morph in autofit mode, if it changes the width, will cause the text to be recomposed with the new width, and then the bounds will be reset to the minimum enclosing rectangle.  Similarly, if the text contents are changed with the wrapFlag set to true, word wrap will be performed based on the current compostion width, after which the bounds will be set (or not), based on the autoFitcontents property.

Note that fonts can only be applied to the TextMorph as a whole.  While you can change the size, color, and emphasis of a subsection of the text and have it apply to only that subsection, changing the font changes the font for the entire contents of the TextMorph. 

Still a TextMorph can be composed of several texts of different fonts
| font1 font2 t1 t2 tMorph|
tMorph := TextMorph new.
font1 := (TextFontReference toFont: (StrikeFont familyName: 'Atlanta' size: 22)).
font2 := (TextFontReference toFont: (StrikeFont familyName: 'Atlanta' size: 11)).
t1 := 'this is font1' asText addAttribute: font1.
t2 := ' and this is font2' asText addAttribute: font2.
tMorph contents: (t1,t2).
tMorph openInHand.


Yet to do:
Make a comprehensive control for the eyedropper, with border width and color, inner color and text color, and margin widths.