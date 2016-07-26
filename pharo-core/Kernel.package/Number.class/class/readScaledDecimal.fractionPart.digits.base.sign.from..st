deprecated
readScaledDecimal: integerPart fractionPart: fractionPart digits: fractionDigits base: base sign: sign from: aStream 
	"Complete creation of a ScaledDecimal, reading scale from aStream. Answer
	a ScaledDecimal, or nil if parsing fails.
	<number>s[<scale>]"

	| scale decimalMultiplier decimalFraction |
	aStream atEnd ifTrue: [^ nil].
	(aStream next == $s) ifFalse: [^ nil].
	"<number>s<scale>"
	(aStream atEnd not and: [aStream peek digitValue between: 0 and: 9])
		ifTrue: [scale := Integer readFrom: aStream]
		ifFalse: [^ nil].
	scale 
		ifNil: ["<number>s"
			fractionDigits = 0
				ifTrue: ["<integer>s"
					scale := 0]
				ifFalse: ["<integer>.<fraction>s"
					scale := fractionDigits]].
	fractionPart 
		ifNil: [^integerPart * sign asScaledDecimal: scale]
		ifNotNil: [decimalMultiplier := base raisedTo: fractionDigits.
			decimalFraction := integerPart * decimalMultiplier + fractionPart * sign / decimalMultiplier.
			^decimalFraction asScaledDecimal: scale]