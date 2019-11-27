"
I represent a symbol, there might be a method with this selector
"
Class {
	#name : #NECSymbolEntry,
	#superclass : #NECEntry,
	#category : #'NECompletion-Model'
}

{ #category : #private }
NECSymbolEntry >> browse [
	| class |
	class := node propertyAt: #type ifAbsent: nil.
	class ifNil: [ SystemNavigation new browseAllImplementorsOf: contents. ^true].
	
	^ self
		findMethodAndDo: [ :method | 
			Smalltalk tools browser 
				openOnClass: method methodClass 
				selector: method selector. 
			true ]
		ifAbsent: [ :selector | 
			SystemNavigation new 
				browseAllImplementorsOf: selector.
			true ]
]

{ #category : #'detail information' }
NECSymbolEntry >> createDescription [
	"Creates description either from looking up the method in a class,
	or in the system."

	^ self
		findMethodAndDo: [:method | 
			self
				methodSourceDescription: method methodClass
				method: method ]
		ifAbsent: [:selector | self implementorsDescription: selector]
]

{ #category : #'detail information' }
NECSymbolEntry >> findMethodAndDo: foundBlock ifAbsent: notfoundBlock [ 
	| result  implementors |
	
	implementors := self systemNavigation allImplementorsOf: contents.
	implementors size == 1
						ifTrue: [| ref | 
							ref := implementors first.
							result := ref realClass lookupSelector: ref selector]
						ifFalse: [^ notfoundBlock value: contents].
			
	^ foundBlock value: result
]

{ #category : #private }
NECSymbolEntry >> implementorsDescription: aSymbol [ 
	| implementors output |
	output := WriteStream on: String new.
	implementors := self systemNavigation allImplementorsOf: aSymbol.
	implementors isEmpty ifTrue: 
		[ ^ NECEntryDescription 
			label: 'symbol'
			title: '(no implementors)'
			description: 'This is just symbol.' ].
	implementors do: [ :each | 
		output
			nextPutAll: each className printString;
			cr ].
	^ NECEntryDescription 
		label: self label
		title: '(Implementors)'
		description: output contents
]

{ #category : #private }
NECSymbolEntry >> methodSourceDescription: aClass method: aCompiledMethod [ 
	"I return if it's a class or a method, its class name, and the implementation."
	| styler styledText |
	
	styler := SHRBTextStyler new.
	styler classOrMetaClass: aClass.
	styledText := styler styledTextFor: (aCompiledMethod sourceCode) asText.
	
	^ NECEntryDescription
		label: self label
		title: aClass printString
		description: styledText
]
