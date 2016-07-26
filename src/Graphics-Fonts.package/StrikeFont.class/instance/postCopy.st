copying
postCopy
 " the receiver is a just created shallow copy. This method gives it the final touch. " 
 
    glyphs := glyphs copy.
    xTable := xTable copy.
    characterToGlyphMap := characterToGlyphMap copy.
 
    self reset.  " takes care of the derivative fonts "