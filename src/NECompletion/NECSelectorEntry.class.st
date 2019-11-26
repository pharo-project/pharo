"
I represent a selector of a method
"
Class {
	#name : #NECSelectorEntry,
	#superclass : #NECSymbolEntry,
	#category : #'NECompletion-Model'
}

{ #category : #'ui related' }
NECSelectorEntry >> browse [
	| class |
	class := node receiver propertyAt: #type ifAbsent: nil.
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

{ #category : #accessing }
NECSelectorEntry >> label [
	"I return whether the variable is a class or a method."
	
	 ^node receiver 
		propertyAt: #type 
		ifPresent:  [ 'method' ] 
		ifAbsent:  [ 'class' ]
]
