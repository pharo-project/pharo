"
I attach a timestamp as a suffix to the name (after last _).
"
Class {
	#name : #OmTimeStampSuffixStrategy,
	#superclass : #OmSuffixAfterDotStrategy,
	#category : #Ombu-Strategies
}

{ #category : #accessing }
OmTimeStampSuffixStrategy >> nextSuffix [

	^ DateAndTime now asNanoSeconds printStringHex
]
