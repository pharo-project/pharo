Class {
	#name : #MetacelloBaselineOfMethodSpec,
	#superclass : #MetacelloVersionMethodSpec,
	#category : #Metacello-Core-Constructors
}

{ #category : #'method generation' }
MetacelloBaselineOfMethodSpec >> methodSelectorAndPragma: aSelector imports: importList on: strm [
  strm
    nextPutAll: aSelector asString , ' spec';
    cr;
    tab;
    nextPutAll: '<baseline>'
]

{ #category : #'method generation' }
MetacelloBaselineOfMethodSpec >> methodSource [
  | strm |
  strm := WriteStream on: String new.
  self methodSelectorAndPragma: self selector imports: self imports on: strm.
  self
    methodSection: self
    pre: [ :methodSection :indent | 
      strm
        cr;
        tab: indent;
        nextPutAll: 'spec for: ' , methodSection attributePrintString , ' do: [';
        cr.
      methodSection versionSpec
        configMethodOn: strm
        last: methodSection methodSections isEmpty
        indent: indent + 1 ]
    last: false
    post: [ :methodSection :indent :last | 
      strm nextPutAll: ' ].'.
      (last
        or: [ indent = 1 or: [ methodSection methodSections isEmpty and: [ indent = 1 ] ] ])
        ifTrue: [ strm cr ] ]
    indent: 0.
  ^ strm contents
]

{ #category : #accessing }
MetacelloBaselineOfMethodSpec >> versionString [
  ^ MetacelloMCBaselineProject singletonVersionName
]
