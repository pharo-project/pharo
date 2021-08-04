"
I represent the abstract superclass of all checksum algorithms.
"
Class {
	#name : #Checksum,
	#superclass : #Object,
	#category : #'System-Hashing-Checksums'
}

{ #category : #testing }
Checksum class >> isAbstract [

	^ self == Checksum
]
