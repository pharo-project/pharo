Class {
	#name : #MetacelloSymbolicVersionMethodSpec,
	#superclass : #MetacelloMethodSpec,
	#category : 'Metacello-Core-Constructors'
}

{ #category : #adding }
MetacelloSymbolicVersionMethodSpec >> addMethodSection: attribute versionString: aString [
    self methodSections
        add:
            (MetacelloSymbolicVersionSpec new
                attributes: attribute asMetacelloAttributeList;
                versionString: aString;
                yourself)
]

{ #category : #'method generation' }
MetacelloSymbolicVersionMethodSpec >> methodSource [
    | strm |
    strm := WriteStream on: String new.
    self symbolicMethodSelectorAndPragma: self selector symbolicVersionSymbol: self versionString on: strm.
    (self methodSections asArray sort: [ :a :b | a attributes first <= b attributes first ])
        do: [ :methodSection | 
            | vs |
            strm
                cr;
                tab;
                nextPutAll: 'spec for: ' , methodSection attributePrintString , ' version: '.
            vs := methodSection versionString.
            vs isSymbol
                ifTrue: [ strm nextPut: $# ].
            strm
                nextPutAll: vs asString printString , '.';
                cr ].
    ^ strm contents
]

{ #category : #'method generation' }
MetacelloSymbolicVersionMethodSpec >> symbolicMethodSelectorAndPragma: aSelector symbolicVersionSymbol: symbolicVersionSymbol on: strm [
	strm
		nextPutAll: aSelector asString , ' spec';
		cr;
		tab;
		nextPutAll: '<symbolicVersion: #' , symbolicVersionSymbol asString printString;
		nextPutAll: '>';
		cr
]

{ #category : #accessing }
MetacelloSymbolicVersionMethodSpec >> versionString: aStringOrSymbol [

	aStringOrSymbol isSymbol ifFalse: [ self error: 'Version symbol ', aStringOrSymbol printString, ' for symbolic version method must be a Symbol' ].
	super versionString: aStringOrSymbol
]
