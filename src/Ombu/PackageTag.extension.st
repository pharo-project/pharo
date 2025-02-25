Extension { #name : 'PackageTag' }

{ #category : '*Ombu-Ston' }
PackageTag class >> fromSton: stonReader [
	"Specialize generic implementation since #new is forbidden in this class."

	^ self basicNew
		fromSton: stonReader;
		yourself
]
