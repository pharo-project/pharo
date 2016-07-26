font lookup
bestFontFor: aLogicalFont whenFindingAlternativeIgnoreAll: ignoreSet
	"look up best real font from the receivers fontProviders.
	If we can't find a font, then answer an alternative real font.
	
	ignoreSet contains the LogicalFonts that we have already attempted to
	get an alternative real font from. We ignore those on each iteration so that we don't 
	recurse forever"
	| textStyle font |
	
	aLogicalFont familyNames do:[:familyName |
		fontProviders do:[:p | | answer |
			(answer := p fontFor: aLogicalFont familyName: familyName)
				ifNotNil:[^answer]].
		textStyle := TextStyle named: familyName.
		textStyle ifNotNil:[
			font := textStyle fontOfPointSize: aLogicalFont pointSize.
			font ifNotNil:[^font emphasized: aLogicalFont emphasis]]].
	"not found, so use the default TextStyle"
	textStyle := TextStyle default.
	textStyle ifNotNil:[
		font := textStyle fontOfPointSize: aLogicalFont pointSize.
		(font isKindOf: LogicalFont) ifFalse:[^font emphasized: aLogicalFont emphasis].
		(ignoreSet includes: font)
			ifFalse:[
				ignoreSet add: font.  "remember that we have visited font so that we don't loop forever"
				"try again using the default TextStyle's logicalFont"
				^self bestFontFor: font whenFindingAlternativeIgnoreAll: ignoreSet]].
	"Neither the family, nor any of the fallback families, is available. 
	Any non-LogicalFont will do as a fallback"
	(TextSharedInformation select: [:each | each isKindOf: TextStyle]) 
		do:[:ts | 
			((font := ts fontOfPointSize: aLogicalFont pointSize) isKindOf: LogicalFont)
				ifFalse:[^font emphasized:  aLogicalFont emphasis]].
	"There are no non-logical fonts in TextSharedInformation - let it fail by answering nil"
	^nil
	
			
