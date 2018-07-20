Extension { #name : #SmallInteger }

{ #category : #'*NewMethodFinder' }
SmallInteger class >> approvedSelectors [
	
	^ #(hashMultiply hightBitOfPositiveReceiver isLarge rfMinus: rfPlus: threeDigitName unpackFromArity:
	   )
]
