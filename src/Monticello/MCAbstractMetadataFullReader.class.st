"
I am an abstract reader for formats that are dealing with some metadatas such as MCZ or FileTree
"
Class {
	#name : 'MCAbstractMetadataFullReader',
	#superclass : 'MCVersionReader',
	#instVars : [
		'infoCache'
	],
	#category : 'Monticello-Storing',
	#package : 'Monticello',
	#tag : 'Storing'
}

{ #category : 'testing' }
MCAbstractMetadataFullReader class >> isAbstract [

	^ self = MCAbstractMetadataFullReader
]

{ #category : 'converting' }
MCAbstractMetadataFullReader >> associate: tokens [
	| result |
	result := Dictionary new.
	tokens pairsDo: [:key :value | 
					| tmp |
					tmp := value.
					value isString ifFalse: [tmp := value collect: [:ea | self associate: ea]].
					value = 'nil' ifTrue: [tmp := ''].
					result at: key put: tmp].
	^ result
]

{ #category : 'utilities' }
MCAbstractMetadataFullReader >> extractInfoFrom: dict [

	^ MCWorkingCopy infoFromDictionary: dict cache: self infoCache
]

{ #category : 'accessing' }
MCAbstractMetadataFullReader >> infoCache [

	^ infoCache ifNil: [ infoCache := Dictionary new ]
]

{ #category : 'loading' }
MCAbstractMetadataFullReader >> loadPackage [
	| dict |
	dict := self parseMember: 'package'.
	package := MCPackage named: (dict at: #name)
]

{ #category : 'loading' }
MCAbstractMetadataFullReader >> parseMember: memberOrName [

	self subclassResponsibility
]
