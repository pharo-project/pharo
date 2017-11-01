Extension { #name : #VirtualMachine }

{ #category : #'*GT-Spotter' }
VirtualMachine >> canHandleShiftPressed [
	^ self interpreterSourceDate < 2015 asYear 
]
