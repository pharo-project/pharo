"
I represent a symbol, there might be a method with this selector
"
Class {
	#name : 'CoSymbolEntry',
	#superclass : 'CoEntry',
	#category : 'NECompletion-Model',
	#package : 'NECompletion',
	#tag : 'Model'
}

{ #category : 'private' }
CoSymbolEntry >> browseCompletionItem [
	| class |
	class := node propertyAt: #type ifAbsent: nil.
	class ifNil: [ SystemNavigation new browseAllImplementorsOf: self selector. ^true].

	^ self
		findMethodAndDo: [ :method |
			(Smalltalk tools toolNamed: #browser)
				openOnClass: method methodClass
				selector: method selector.
			true ]
		ifAbsent: [ :selector |
			SystemNavigation new
				browseAllImplementorsOf: selector.
			true ]
]

{ #category : 'detail information' }
CoSymbolEntry >> createDescription [
	"Creates description either from looking up the method in a class,
	or in the system."

	^ self
		findMethodAndDo: [ :method |
			self
				methodSourceDescription: method methodClass
				method: method ]
		ifAbsent: [ :selector | self implementorsDescription: selector]
]

{ #category : 'private' }
CoSymbolEntry >> emptyLabel [
	
	| stringForEmpty |
	stringForEmpty := self rawLabel.
	self originDisplayed ifTrue: [ stringForEmpty := stringForEmpty, ' from: ', self fetcher ].
	^ stringForEmpty
]

{ #category : 'detail information' }
CoSymbolEntry >> findMethodAndDo: foundBlock ifAbsent: notfoundBlock [
	| result  implementors |

	implementors := self systemNavigation allImplementorsOf: self selector.
	implementors size == 1
						ifTrue: [| ref |
							ref := implementors first.
							result := ref realClass lookupSelector: ref selector]
						ifFalse: [^ notfoundBlock value: self selector ].

	^ foundBlock value: result
]

{ #category : 'private' }
CoSymbolEntry >> implementorsDescription: aSymbol [
	| implementors output |
	output := WriteStream on: String new.
	implementors := self systemNavigation allImplementorsOf: aSymbol.
	implementors isEmpty 
		ifTrue: [ ^ CoEntryDescription
			label: self emptyLabel 
			title: '(no implementors)'
			description: 'This is just symbol.' ].

	implementors do: [ :each |
		output
			nextPutAll: each methodClass printString;
			cr ].

	^ CoEntryDescription
		label: self label
		title: '(Implementors)'
		description: output contents
]

{ #category : 'private' }
CoSymbolEntry >> methodSourceDescription: aClass method: aCompiledMethod [
	"I return if it's a class or a method, its class name, and the implementation."
	
	| styler styledText |

	styler := SHRBTextStyler new.
	styler classOrMetaClass: aClass.
	styledText := styler styledTextFor: (aCompiledMethod sourceCode) asText.

	^ CoEntryDescription
		label: self label
		title: aClass printString
		description: styledText
]

{ #category : 'private' }
CoSymbolEntry >> rawLabel [
	
	^ 'symbol'
]

{ #category : 'accessing' }
CoSymbolEntry >> selector [

	^ contents
]
