"
I am a raw data layout that holds double words (64 bit).
"
Class {
	#name : 'DoubleWordLayout',
	#superclass : 'BitsLayout',
	#category : 'Kernel-CodeModel-Layout',
	#package : 'Kernel-CodeModel',
	#tag : 'Layout'
}

{ #category : 'instance creation' }
DoubleWordLayout class >> extending: superLayout scope: aScope host: aClass [
	^ superLayout extendDoubleWord
		host: aClass;
		yourself
]

{ #category : 'description' }
DoubleWordLayout class >> subclassDefiningSymbol [
	"Answer a keyword that describes the receiver's kind of subclass
	Note: this is for printing the ST80 style class definiton, usage makes no sense for anything
	else!"
	^#variableDoubleWordSubclass:
]

{ #category : 'extending' }
DoubleWordLayout >> extendDoubleWord [
	^ DoubleWordLayout new
]

{ #category : 'format' }
DoubleWordLayout >> instanceSpecification [
	^ 9
]

{ #category : 'testing' }
DoubleWordLayout >> isDoubleWords [
	^ true
]
