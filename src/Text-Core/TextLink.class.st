"
A link in text referring to class and methods. 
"
Class {
	#name : #TextLink,
	#superclass : #TextAction,
	#instVars : [
		'classAndMethod'
	],
	#category : #'Text-Core-Attributes'
}

{ #category : #'instance creation' }
TextLink class >> scanFrom: strm [
	"read a link in the funny format used by Text styles on files. LPoint +;LPoint Comment;"

	^ self new classAndMethod: (strm upTo: $;)
]

{ #category : #comparing }
TextLink >> = anotherObject [

	^self class == anotherObject class and: [ classAndMethod = anotherObject info ]
]

{ #category : #testing }
TextLink >> analyze: aString [

	| list |
	list := super analyze: aString.
	classAndMethod := list at: 1.
	^ list at: 2
]

{ #category : #testing }
TextLink >> analyze: aString with: nonMethod [
	"Initalize this attribute holder with a piece text the user typed into a paragraph.  Returns the text to emphesize (may be different from selection)  Does not return self!.  nonMethod is what to show when clicked, i.e. the last part of specifier (Comment, Definition, or Hierarchy).  May be of the form:
Point
<Point>
Click Here<Point>
<Point>Click Here
"
	"Obtain the showing text and the instructions"
	| b1 b2 trim |
	b1 := aString indexOf: $<.
	b2 := aString indexOf: $>.
	(b1 < b2) & (b1 > 0) ifFalse: ["only one part"
		classAndMethod := self validate: aString, ' ', nonMethod.
		^ classAndMethod ifNotNil: [aString]].
	"Two parts"
	trim := aString trimBoth.
	(trim at: 1) == $< 
		ifTrue: [(trim last) == $>
			ifTrue: ["only instructions" 
				classAndMethod := self validate: (aString copyFrom: b1+1 to: b2-1), ' ', nonMethod.
				^ classAndMethod ifNotNil: [classAndMethod]]
			ifFalse: ["at the front"
				classAndMethod := self validate: (aString copyFrom: b1+1 to: b2-1), ' ', nonMethod.
				^ classAndMethod ifNotNil: [aString copyFrom: b2+1 to: aString size]]]
		ifFalse: [(trim last) == $>
			ifTrue: ["at the end"
				classAndMethod := self validate: (aString copyFrom: b1+1 to: b2-1), ' ', nonMethod.
				^ classAndMethod ifNotNil: [aString copyFrom: 1 to: b1-1]]
			ifFalse: ["Illegal -- <> has text on both sides"
				^ nil]]

]

{ #category : #accessing }
TextLink >> classAndMethod: aString [
	classAndMethod := aString
]

{ #category : #comparing }
TextLink >> hash [

	^(self class hash + classAndMethod hash) hashMultiply 
]

{ #category : #accessing }
TextLink >> info [
	^ classAndMethod
]

{ #category : #testing }
TextLink >> mayHaveExternalReferences [
	"Text links can keep references to external objects like AST nodes"
	^ true
]

{ #category : #testing }
TextLink >> validate: specString [
	"Can this string be decoded to be Class space Method (or Comment, Definition, Hierarchy)? If so, return it in valid format, else nil"

	| list first mid last |
	list := specString findTokens: ' 	.|'.
	list ifEmpty: [ ^ nil ].
	last := list last.
	last first isUppercase
		ifTrue: [ 
			(#('Comment' 'Definition' 'Hierarchy') includes: last)
				ifFalse: [ ^ nil ].	"Check for 'Rectangle Comment Comment' and remove last one"
			(list at: list size - 1 ifAbsent: [ ^ nil ]) = last
				ifTrue: [ list := list allButLast ] ].
	list size > 3
		ifTrue: [ ^ nil ].
	list size < 2
		ifTrue: [ ^ nil ].
	Symbol hasInterned: list first ifTrue: [ :sym | first := sym ].
	first ifNil: [ ^ nil ].
	Smalltalk globals at: first ifAbsent: [ ^ nil ].
	mid := list size = 3
		ifTrue: [ 
			(list at: 2) = 'class'
				ifTrue: [ 'class ' ]
				ifFalse: [ ^ nil ] ]
		ifFalse: [ '' ].	"OK if method name is not interned -- may not be defined yet"
	^ first , ' ' , mid , last
]
