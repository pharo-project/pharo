font lookup
fontFor: aLogicalFont familyName: familyName
	| info answer simulatedSqueakEmphasis needsSimulatedBold needsSimulatedSlant
	squeakBoldEmphasis squeakItalicEmphasis |
	
	FT2Library current ifNil:[^nil].
	info:= self fontInfoFor: aLogicalFont familyName: familyName.
	info ifNil:[^nil].
	answer := FreeTypeFont forLogicalFont: aLogicalFont fileInfo: info.
	needsSimulatedBold := aLogicalFont isBoldOrBolder and:[(info isBolderThan: 500) not].
	needsSimulatedSlant := aLogicalFont isItalicOrOblique and: [info isItalicOrOblique not].
	(needsSimulatedBold or:[needsSimulatedSlant])
		ifTrue:[
			squeakBoldEmphasis := 1.
			squeakItalicEmphasis := 2.
			simulatedSqueakEmphasis := 0.
			needsSimulatedBold 
				ifTrue:[
					simulatedSqueakEmphasis := simulatedSqueakEmphasis + squeakBoldEmphasis].
			needsSimulatedSlant 
				ifTrue:[
					simulatedSqueakEmphasis := simulatedSqueakEmphasis + squeakItalicEmphasis].
			answer simulatedEmphasis: simulatedSqueakEmphasis].
	answer face validate.
	answer face isValid ifFalse:[^nil].  "we may get this if startup causes text display BEFORE receiver has been updated from the system"
	^answer