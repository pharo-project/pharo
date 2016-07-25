accessing - other properties
sortedPropertyNames
	"answer the receiver's property names in a sorted way"

	| props |
	props := (Array new: 10) writeStream.
	locked == true ifTrue: [props nextPut: #locked].
	visible == false ifTrue: [props nextPut: #visible].
	sticky == true ifTrue: [props nextPut: #sticky].
	balloonText ifNotNil: [props nextPut: #balloonText].
	externalName ifNotNil: [props nextPut: #externalName].
	eventHandler ifNotNil: [props nextPut: #eventHandler].
	 otherProperties ifNotNil: [otherProperties associationsDo: [:a | props nextPut: a key]].
	^props contents sort: [:s1 :s2 | s1 <= s2]