"
I attach random characters as a suffix to the name (after last _).
"
Class {
	#name : #OmRandomSuffixStrategy,
	#superclass : #OmSuffixAfterDotStrategy,
	#category : #'Ombu-Strategies'
}

{ #category : #accessing }
OmRandomSuffixStrategy >> nextSuffix [

	^ UUID new asString36
]
