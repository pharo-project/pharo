Extension { #name : #Fraction }

{ #category : #'*NewMethodFinder' }
Fraction class >> approvedSelectors [

	 ^ #(denominator numerator reduced setNumerator:denominator:
	     isPowerOfTwo
	     asLargerPowerOfTwo asSmallerOfTwo 
	     )
]
