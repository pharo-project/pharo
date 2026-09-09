"
I represent a selector of a method
"
Class {
	#name : 'CoSelectorEntry',
	#superclass : 'CoSymbolEntry',
	#instVars : [
		'selector'
	],
	#category : 'NECompletion-Model',
	#package : 'NECompletion',
	#tag : 'Model'
}

{ #category : 'ui related' }
CoSelectorEntry >> browseCompletionItem [

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
CoSelectorEntry >> findMethodAndDo: foundBlock ifAbsent: notfoundBlock [

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
CoSelectorEntry >> implementorsDescription: aSymbol [
	| implementors output |
	output := WriteStream on: String new.
	implementors := self systemNavigation allImplementorsOf: aSymbol.
	implementors isEmpty 
		ifTrue: [ ^ CoEntryDescription
			label: self emptyLabel 
			title: '(no implementors)'
			description: 'This is just a symbol.' ].

	implementors do: [ :each |
		output
			nextPutAll: each methodClass printString;
			cr ].
	
	^ CoEntryDescription
		label: self label
		title: '(Implementors)'
		description: output contents
]

{ #category : 'accessing' }
CoSelectorEntry >> label [

	node isMessage ifFalse: [ ^ '' ].
	^ super label.
]

{ #category : 'accessing' }
CoSelectorEntry >> rawLabel [

	self node isMethod 
		ifTrue: [ ^ 'method' ].
	^ self node receiver
		propertyAt: #type
		ifPresent:  [ 'method' ]
		ifAbsent:  [ 'class' ]

]

{ #category : 'accessing' }
CoSelectorEntry >> selector [

	^ selector ifNil: [ contents ]
]

{ #category : 'accessing' }
CoSelectorEntry >> selector: anObject [

	selector := anObject
]
