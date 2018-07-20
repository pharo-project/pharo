Extension { #name : #Number }

{ #category : #'*NewMethodFinder' }
Number class >> approvedSelectors [

	^ #(% * ** + - / // @ \\ abs negated quo: reciprocal rem:
	    adaptToCollection:andSend: asFloat asInteger asMargin asNumber asPoint asSclaedDecimal asScaledDecimal:       
	       asSmallAngleDegrees asSmallPositiveDegrees copySignTo: sign sign:
	    exp raisedTo: raisedToFraction: raisedToInteger: signBit
	    isInfinite isNaN isZero negative positive
	    ceiling detentBy:atMultiplesOf:snap: floor fractionPart integerPart reduce round: roundDownTo: roundTo: roundUpTo: 
	       rounded truncateTo: truncated
	    to: to:by: to:by:do: to:do:
	    arcCos arcSin arcTan arcTan: closeTo:precision: cos degreeCos degreeSin degreesToradians even floorLog: interpolateTo:at:
	       isDivisibleBy: ln log nthRoot: odd percent radiansToDegrees sin sqrt squared stritlyPositive tan
	)
]
