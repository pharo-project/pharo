Extension { #name : #String }

{ #category : #'*Slot-Core' }
String >> asClassVariable [
	^ self asSymbol asClassVariable
]

{ #category : #'*Slot-Core' }
String >> asSlot [
	^ self asSymbol asSlot
]

{ #category : #'*Slot-Core' }
String >> asSlotCollection [
	"Parse as slots. Use space, tab and cr as separators
	Example: 
	  'a b' asSlotCollection --> {a => Slot. b => Slot}
	"
	| whitespaces |
	whitespaces := ByteString withAll: { Character space . Character tab . Character cr }.
	^(self substrings: whitespaces) collect: [ :substring | substring asSlot ]
]
