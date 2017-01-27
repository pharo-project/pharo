tests - object
testMutateIVObject
	| guineaPig |
	guineaPig := MessageSend new.
	guineaPig beReadOnlyObject.
	[ guineaPig receiver: 1 ] 
		on: NoModificationError 
		do: [ "Surely a NoModification error" ].
	guineaPig
		beWritableObject;
		selector: #+;
		beReadOnlyObject.
	[ guineaPig arguments: #(2) ] 
		on: NoModificationError 
		do: [ "Surely a NoModification error" ].
	self assert: guineaPig receiver isNil.
	self assert: guineaPig arguments isNil.
	self assert: guineaPig selector == #+.