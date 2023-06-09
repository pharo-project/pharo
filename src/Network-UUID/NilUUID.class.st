"
UUID with all fields set to 0.
"
Class {
	#name : #NilUUID,
	#superclass : #UUID,
	#category : #'Network-UUID-Base'
}

{ #category : #initialization }
NilUUID >> initialize [
	uuidData := ByteArray new: 16.
]
