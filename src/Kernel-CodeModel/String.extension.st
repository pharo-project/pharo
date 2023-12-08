Extension { #name : 'String' }

{ #category : '*Kernel-CodeModel' }
String >> asClassVariable [
	^ ClassVariable named: self
]

{ #category : '*Kernel-CodeModel' }
String >> asClassVariableCollection [
	"Parse as class variables. Use space, tab and cr as separators
	Example:
	  'A B' asClassVariableCollection --> {{#a => ClassVariable. #b => ClassVariable}
	"

	^(self substrings: Character separators) collect: [ :substring | substring asSymbol asClassVariable ]
]

{ #category : '*Kernel-CodeModel' }
String >> asSlot [
	^ InstanceVariableSlot named: self
]

{ #category : '*Kernel-CodeModel' }
String >> asSlotCollection [
	"Parse as slots. Use space, tab and cr as separators
	Example:
	  'a b' asSlotCollection --> {a => Slot. b => Slot}
	"

	^(self substrings: Character separators) collect: [ :substring | substring asSymbol asSlot ]
]
