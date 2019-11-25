"
I represent a selector
"
Class {
	#name : #NECSelectorEntry,
	#superclass : #NECEntry,
	#category : #'NECompletion-Model'
}

{ #category : #'ui related' }
NECSelectorEntry >> browse [
	| class |
	class := node receiver propertyAt: #type ifAbsent: nil.
	class ifNil: [ SystemNavigation new browseAllImplementorsOf: contents].
	
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
NECSelectorEntry >> createDescription [
	"Creates description either from looking up the method in a class,
	or in the system."

	^ self
		findMethodAndDo: [:method | 
			self
				methodSourceDescription: method methodClass
				method: method ]
		ifAbsent: [:selector | self implementorsDescription: selector]
]

{ #category : #private }
NECSelectorEntry >> findMethodAndDo: foundBlock ifAbsent: notfoundBlock [ 
	| theClass result implementors |
	theClass := node receiver propertyAt: #type ifAbsent: nil.
	result := theClass
				ifNil: [implementors := self systemNavigation allImplementorsOf: contents.
					implementors size == 1
						ifTrue: [| ref | 
							ref := implementors first.
							ref realClass lookupSelector: ref selector]
						ifFalse: [^ notfoundBlock value: contents]]
				ifNotNil: [theClass lookupSelector: contents ]. 
	^ foundBlock value: result
]

{ #category : #private }
NECSelectorEntry >> implementorsDescription: aSymbol [ 
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

{ #category : #accessing }
NECSelectorEntry >> label [
	"I return whether the variable is a class or a method."
	
	 ^node receiver 
		propertyAt: #type 
		ifPresent:  [ 'method' ] 
		ifAbsent:  [ 'class' ]
]

{ #category : #private }
NECSelectorEntry >> methodSourceDescription: aClass method: aCompiledMethod [ 
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
