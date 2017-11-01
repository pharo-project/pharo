Extension { #name : #RPackageTag }

{ #category : #'*Ombu-Ston' }
RPackageTag class >> fromSton: stonReader [
	"Specialize generic implementation since #new is forbidden in this class."
	
	^ self basicNew
		fromSton: stonReader;
		yourself
]
