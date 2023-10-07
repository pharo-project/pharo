"
I represent the abstract superclass of all checksum algorithms.
"
Class {
	#name : 'Checksum',
	#superclass : 'Object',
	#category : 'System-Hashing-Checksums',
	#package : 'System-Hashing',
	#tag : 'Checksums'
}

{ #category : 'testing' }
Checksum class >> isAbstract [

	^ self == Checksum
]
