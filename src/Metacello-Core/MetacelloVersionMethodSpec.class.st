Class {
	#name : #MetacelloVersionMethodSpec,
	#superclass : #MetacelloMethodSpec,
	#instVars : [
		'imports'
	],
	#category : 'Metacello-Core-Constructors'
}

{ #category : #adding }
MetacelloVersionMethodSpec >> addMethodSection: attributePath versionSpec: versionSpec [
    | attributeList methodSection index sections found |
    attributeList := attributePath last.
    methodSection := MetacelloVersionMethodSection new
        attributes: attributeList;
        versionSpec: versionSpec;
        yourself.
    attributePath size > 1
        ifTrue: [ 
            index := 1.
            sections := self methodSections.
            found := true.
            [ found ]
                whileTrue: [ 
                    found := false.
                    sections
                        do: [ :ms | 
                            ms attributes = (attributePath at: index)
                                ifTrue: [ 
                                    index == (attributePath size -1)
                                        ifTrue: [ 
                                            ms methodSections add: methodSection.
                                            ^ self ].
                                    sections := ms methodSections.
                                    index := index + 1.
                                    found := true ] ] ].
            self error: 'Method section for attribute: ' , (attributePath at: index) printString , ' not found.' ]
        ifFalse: [ self methodSections add: methodSection ]
]

{ #category : #adding }
MetacelloVersionMethodSpec >> findMethodSection: attributeOrPath sectionIndex: sectionIndex [
    | attributePath index sections found |
    attributePath := attributeOrPath asMetacelloAttributePath.
    index := 1.
    sections := self methodSections.
    found := true.
    [ found ]
        whileTrue: [ 
            found := false.
            sections
                do: [ :ms | 
                    ms attributes = (attributePath at: index)
                        ifTrue: [ 
                            index == attributePath size
                                ifTrue: [ ^ ms ].
                            sections := ms methodSections.
                            index := index + 1.
                            found := true ] ] ].
    self error: 'Method section for attribute: ' , attributePath printString , ' not found.'
]

{ #category : #accessing }
MetacelloVersionMethodSpec >> imports [
	imports == nil ifTrue: [ imports := #() ].
	^ imports
]

{ #category : #accessing }
MetacelloVersionMethodSpec >> imports: anObject [
	imports := anObject
]

{ #category : #private }
MetacelloVersionMethodSpec >> methodSection: methodSection pre: preBlock last: last post: postBlock indent: indent [
	| list |
	methodSection ~~ self
		ifTrue: [ preBlock value: methodSection value: indent ].
	list := methodSection methodSections.
	1 to: list size
		do: [ :index | | ms | 
			ms := list at: index.
			self
				methodSection: ms
				pre: preBlock
				last: index ~= list size
				post: postBlock
				indent: indent + 1 ].
	methodSection ~~ self
		ifTrue: [ postBlock value: methodSection value: indent value: last ]
]

{ #category : #'method generation' }
MetacelloVersionMethodSpec >> methodSelectorAndPragma: aSelector imports: importList versionString: aString on: strm [
	strm
		nextPutAll: aSelector asString , ' spec';
		cr;
		tab;
		nextPutAll: '<version: ' , aString printString.
	importList notEmpty
		ifTrue: [ 
			strm nextPutAll: ' imports: #('.
			importList
				do: [ :importVersionString | 
					strm
						nextPutAll: importVersionString printString;
						space ].
			strm nextPut: $) ].
	strm
		nextPutAll: '>';
		cr
]

{ #category : #'method generation' }
MetacelloVersionMethodSpec >> methodSource [
    | strm |
    strm := WriteStream on: String new.
    self
        methodSelectorAndPragma: self selector
        imports: self imports
        versionString: self versionString
        on: strm.
    self
        methodSection: self
        pre: [ :methodSection :indent | 
            strm
                cr;
                tab: indent;
                nextPutAll: 'spec for: ' , methodSection attributePrintString , ' do: [';
                cr.
            methodSection versionSpec configMethodOn: strm last: methodSection methodSections isEmpty indent: indent + 1 ]
        last: false
        post: [ :methodSection :indent :last | 
            strm nextPutAll: ' ].'.
            (last or: [ indent = 1 or: [ methodSection methodSections isEmpty and: [ indent = 1 ] ] ])
                ifTrue: [ strm cr ] ]
        indent: 0.
    ^ strm contents
]

{ #category : #accessing }
MetacelloVersionMethodSpec >> versionString: aStringOrSymbol [

	aStringOrSymbol isSymbol ifTrue: [ self error: 'Version string ', aStringOrSymbol printString, ' for version method must be a String' ].
	super versionString: aStringOrSymbol
]
