Class {
	#name : 'ZnCertificateVerificationFailed',
	#superclass : 'Error',
	#instVars : [
		'verificationState'
	],
	#category : 'Zinc-HTTP-Exceptions',
	#package : 'Zinc-HTTP',
	#tag : 'Exceptions'
}

{ #category : 'testing' }
ZnCertificateVerificationFailed >> isResumable [
	"Determine whether an exception is resumable."

	^ true
]

{ #category : 'accessing' }
ZnCertificateVerificationFailed >> verificationState [
	^ verificationState
]

{ #category : 'accessing' }
ZnCertificateVerificationFailed >> verificationState: anInteger [
	verificationState := anInteger
]
