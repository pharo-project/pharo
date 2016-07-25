scanning
scanByteCharactersFrom: startIndex to: stopIndex in: sourceString rightX: rightX 
"this is a scanning method for
single byte characters in a ByteString
a font that does not do character-pair kerning"
	^self primScanCharactersFrom: startIndex to: stopIndex in: sourceString rightX: rightX stopConditions: stopConditions kern: kern
