"
I attach random characters as a suffix to the name (after last _).
"
Class {
	#name : 'OmRandomSuffixStrategy',
	#superclass : 'OmSuffixAfterDotStrategy',
	#category : 'Ombu-Strategies',
	#package : 'Ombu',
	#tag : 'Strategies'
}

{ #category : 'accessing' }
OmRandomSuffixStrategy >> nextSuffix [

	^ UUID new asString36
]
