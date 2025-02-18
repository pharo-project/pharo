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
	class ifNil: [ SystemNavigation new browseAllImplementorsOf: self selector. ^true].

	^ self
		findMethodAndDo: [ :method |
			(Smalltalk tools toolNamed: #browser)
				openOnClass: method methodClass
				selector: method selector.
			true ]
		ifAbsent: [ :sel |
			SystemNavigation new
				browseAllImplementorsOf: sel.
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

{ #category : 'accessing' }
NECSelectorEntry >> label [
	node isMessage ifFalse: [ ^ '' ].

	"I return whether the variable is a class or a method."
	 ^node receiver
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
