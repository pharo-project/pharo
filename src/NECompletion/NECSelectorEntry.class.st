"
I represent a selector
"
Class {
	#name : #NECSelectorEntry,
	#superclass : #NECEntry,
	#category : #'NECompletion-Model'
}

{ #category : #private }
NECSelectorEntry >> browseWith: anECContext [

	(self guessTypeWith: anECContext)
		ifNotNil: [ :clazz|
			Smalltalk tools browser openOnClass: clazz.
			^ true ].
	
	^ self
		findMethodWith: anECContext
		do: [ :class :method | 
			Smalltalk tools browser 
				openOnClass: class 
				selector: method selector. 
			true ]
		ifAbsent: [ :selector | 
			SystemNavigation new 
				browseAllImplementorsOf: selector.
			true ]
]

{ #category : #'detail information' }
NECSelectorEntry >> createDescriptionWith: anECContext [

	(self guessTypeWith: anECContext)
		ifNotNil: [ :clazz|
			^ NECEntryDescription 
					label: self label
					title: clazz printString
					description: clazz comment	].
	^ self
		findMethodWith: anECContext
		do: [:clazz :method | 
			self
				methodSourceDescription: clazz
				method: method ]
		ifAbsent: [:selector | self implementorsDescription: selector]
]

{ #category : #private }
NECSelectorEntry >> findMethodWith: anECContext do: foundBlock ifAbsent: notfoundBlock [ 
	| theClass result implementors |
	theClass := anECContext model theClass.
	result := theClass
				ifNil: [implementors := self systemNavigation allImplementorsOf: contents.
					implementors size == 1
						ifTrue: [| ref | 
							ref := implementors first.
							self lookupSelector: ref selector class: ref realClass]
						ifFalse: [^ notfoundBlock value: contents]]
				ifNotNil: [self lookupSelector: contents class: theClass]. 
	^ foundBlock value: result first value: result second
]

{ #category : #operations }
NECSelectorEntry >> guessTypeWith: anECContext [ 
	| globalEntry |
	globalEntry := Smalltalk at: contents ifAbsent: [^ nil].
	globalEntry isBehavior ifTrue: [^ globalEntry].
	globalEntry ifNotNil: [^ globalEntry class].
	^ nil
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
	^ (self guessTypeWith: nil) ifNil:[ 'method' ] ifNotNil: [ 'class' ].
]

{ #category : #private }
NECSelectorEntry >> lookupSelector: aSymbol class: aClass [ 
	"Look up the given selector in my methodDictionary.
	Return the corresponding method if found.
	Otherwise chase the superclass chain and try again.
	Return nil if no method is found."
	| lookupClass |
	lookupClass := aClass.
	[lookupClass isNil]
		whileFalse: [(lookupClass includesSelector: aSymbol)
				ifTrue: [^ Array
						with: lookupClass
						with: (lookupClass compiledMethodAt: aSymbol)].
			lookupClass := lookupClass superclass].
	^ nil
]

{ #category : #private }
NECSelectorEntry >> methodSourceDescription: aClass method: aCompiledMethod [ 
	| styler styledText |
	
	styler := SHRBTextStyler new.
	styler classOrMetaClass: aClass.
	styledText := styler styledTextFor: (aCompiledMethod sourceCode) asText.
	
	^ NECEntryDescription
		label: self label
		title: aClass printString
		description: styledText
]
