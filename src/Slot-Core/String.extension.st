Extension { #name : #String }

{ #category : #'*Slot-Core' }
String >> asClassVariable [
	^ ClassVariable named: self
]

{ #category : #'*Slot-Core' }
String >> asSlot [
	^ InstanceVariableSlot named: self
]

{ #category : #'*Slot-Core' }
String >> asSlotCollection [
	"Parse as slots. Use space, tab and cr as separators
	Example: 
	  'a b' asSlotCollection --> {a => Slot. b => Slot}
	"
	
	^(self substrings: Character separators) collect: [ :substring | substring asSymbol asSlot ]
]
