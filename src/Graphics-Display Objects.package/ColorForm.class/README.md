ColorForm is a normal Form plus a color map of up to 2^depth Colors. Typically, one reserves one entry in the color map for transparent. This allows 1, 3, 15, or 255 non-transparent colors in ColorForms of depths 1, 2, 4, and 8 bits per pixel. ColorForms don't support depths greater than 8 bits because that would require excessively large color maps with little real benefit, since 16-bit and 32-bit depths already support thousands and millions of colors.

ColorForms have several uses:
  1) Precise colors. You can have up to 256 true colors, instead being limited to the 8-bit color palette.
  2) Easy transparency. Just store (Color transparent) at the desired position in the color map.
  3) Cheap color remapping by changing the color map.

A color map is an Array of up to 2^depth Color objects. A Bitmap colorMap is automatically computed and cached for rapid display. Note that if you change the color map, you must resubmit it via the colors: method to flush this cache.

ColorForms can be a bit tricky. Note that:
  a) When you BitBlt from one ColorForm to another, you must remember to copy the color map of the source ColorForm to the destination ColorForm.
  b) A ColorForm's color map is an array of depth-independent Color objects. BitBlt requires a BitMap of actual pixel values, adjusted to the destination depth. These are different things! ColorForms automatically maintain a cache of the BitBlt-style color map corresponding to the colors array for the last depth on which the ColorForm was displayed, so there should be little need for clients to work with BitBlt-style color maps.
  c) The default map for 8 bit depth has black in the first entry, not transparent.  Say (cform colors at: 1 put: Color transparent).
