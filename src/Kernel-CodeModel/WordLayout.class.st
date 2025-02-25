"
I am a raw data layout that holds words (32 bits ).
"
Class {
	#name : 'WordLayout',
	#superclass : 'BitsLayout',
	#category : 'Kernel-CodeModel-Layout',
	#package : 'Kernel-CodeModel',
	#tag : 'Layout'
}

{ #category : 'instance creation' }
WordLayout class >> extending: superLayout scope: aScope host: aClass [
	^ superLayout extendWord
		host: aClass;
		yourself
]

{ #category : 'description' }
WordLayout class >> subclassDefiningSymbol [
	"Answer a keyword that describes the receiver's kind of subclass
	Note: this is for printing the ST80 style class definiton, usage makes no sense for anything
	else!"
	^#variableWordSubclass:
]

{ #category : 'accessing' }
WordLayout >> bytesPerSlot [

	^ 4
]

{ #category : 'extending' }
WordLayout >> extendWord [
	^ WordLayout new
]

{ #category : 'format' }
WordLayout >> instanceSpecification [
	^ 10
]

{ #category : 'testing' }
WordLayout >> isWords [
	^ true
]
