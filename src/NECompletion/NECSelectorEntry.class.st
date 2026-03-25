"
I represent a selector of a method
"
Class {
	#name : 'NECSelectorEntry',
	#superclass : 'NECSymbolEntry',
	#instVars : [
		'selector'
	],
	#category : 'NECompletion-Model',
	#package : 'NECompletion',
	#tag : 'Model'
}

{ #category : 'ui related' }
NECSelectorEntry >> browse [

	| class |
	class := node receiver propertyAt: #type ifAbsent: nil.
	class ifNil: [
			SystemNavigation new browseAllImplementorsOf: self selector.
			^ true ].

	^ self
		  findMethodAndDo: [ :method |
				  (Smalltalk tools toolNamed: #browser) 
						openOnClass: method methodClass selector: method selector.
				  true ]
		  ifAbsent: [ :sel |
				  SystemNavigation new browseAllImplementorsOf: sel.
				  true ]
]

{ #category : 'private' }
NECSelectorEntry >> findMethodAndDo: foundBlock ifAbsent: notfoundBlock [

	| theClass result implementors |
	theClass := node isMessage
		ifTrue: [ node receiver propertyAt: #type ifAbsent: nil ]
		ifFalse: [ nil ].
	result := theClass
				ifNil: [implementors := self systemNavigation allImplementorsOf: self selector.
					implementors size == 1
						ifTrue: [| ref |
							ref := implementors first.
							ref realClass lookupSelector: ref selector]
						ifFalse: [^ notfoundBlock value: self selector]]
				ifNotNil: [theClass lookupSelector: self selector ].
	result ifNil: [^ notfoundBlock value: self selector].
	^ foundBlock value: result
]

{ #category : 'private' }
NECSelectorEntry >> implementorsDescription: aSymbol [
	| implementors output |
	output := WriteStream on: String new.
	implementors := self systemNavigation allImplementorsOf: aSymbol.
	implementors isEmpty 
		ifTrue: [ ^ NECEntryDescription
			label: self emptyLabel 
			title: '(no implementors)'
			description: 'This is just a symbol.' ].

	implementors do: [ :each |
		output
			nextPutAll: each methodClass printString;
			cr ].
	
	^ NECEntryDescription
		label: self label
		title: '(Implementors)'
		description: output contents
]

{ #category : 'accessing' }
NECSelectorEntry >> label [

	node isMessage ifFalse: [ ^ '' ].
	^ super label.
]

{ #category : 'accessing' }
NECSelectorEntry >> rawLabel [

	^ self node receiver
		propertyAt: #type
		ifPresent:  [ 'method' ]
		ifAbsent:  [ 'class' ]

]

{ #category : 'accessing' }
NECSelectorEntry >> selector [

	^ selector ifNil: [ contents ]
]

{ #category : 'accessing' }
NECSelectorEntry >> selector: anObject [

	selector := anObject
]
