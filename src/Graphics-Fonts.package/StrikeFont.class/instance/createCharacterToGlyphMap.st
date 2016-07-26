private
createCharacterToGlyphMap
        "Private. Create the character to glyph mapping for a font that didn't have any before. This is basically equivalent to what the former setStopCondition did, only based on indexes."

        maxAscii < 256 ifTrue: [^ (1 to: 256) collect: [:i | i - 1]].
        ^ nil.
