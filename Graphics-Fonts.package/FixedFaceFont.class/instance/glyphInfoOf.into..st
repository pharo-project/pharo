private
glyphInfoOf: aCharacter into: glyphInfoArray

	^ baseFont glyphInfoOf: substitutionCharacter into: glyphInfoArray.
