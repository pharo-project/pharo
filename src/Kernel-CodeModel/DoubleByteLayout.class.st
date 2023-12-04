"
I am a raw data layout that holds double bytes (16 bits).
"
Class {
	#name : 'DoubleByteLayout',
	#superclass : 'BitsLayout',
	#category : 'Kernel-CodeModel-Layout',
	#package : 'Kernel-CodeModel',
	#tag : 'Layout'
}

{ #category : 'instance creation' }
DoubleByteLayout class >> extending: superLayout scope: aScope host: aClass [
	^ superLayout extendDoubleByte
		host: aClass;
		yourself
]

{ #category : 'description' }
DoubleByteLayout class >> subclassDefiningSymbol [
	"Answer a keyword that describes the receiver's kind of subclass
	Note: this is for printing the ST80 style class definiton, usage makes no sense for anything
	else!"
	^#variableDoubleByteSubclass:
]

{ #category : 'accessing' }
DoubleByteLayout >> bytesPerSlot [

	^ 2
]

{ #category : 'extending' }
DoubleByteLayout >> extendDoubleByte [
	^ DoubleByteLayout new
]

{ #category : 'format' }
DoubleByteLayout >> instanceSpecification [
	^ 12
]

{ #category : 'testing' }
DoubleByteLayout >> isDoubleBytes [
	^ true
]
