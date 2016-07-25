I represent a view for a portion of text, laid out using the specified layout 
strategy.

The view is defined using the following inputs:

- position in the text
- view extent (width and height)
- surface, which will be used to render the view.

I support rendering via #renderOn:, as well as translating between a view point 
and a text position (but only in the area of the view itself).

The surface is responsible for providing the font renderer, which is used to measure text metrics and lay it out properly using the specified strategy, and keep synchronized with what the renderer produces on the surface.

Please note, that the view's layout is computed only for the portion of text neccessary to display the area visible through the view (unless the text fully fits into it), which makes the view completely independent from the size of the text.

The text layout needs to be adjusted/reset when: 
 - view's text position changes
 - view is scrolled up/down (which indirectly changes view's text position)
 - text is modified
 - layout strategy is changed
 - drawing surface is changed 

