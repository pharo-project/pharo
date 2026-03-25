"
This is the unit test for the class CompiledMethod. Unit tests are a good way to exercise the functionality of your system in a repeatable and automatic manner. They are therefore recommended if you plan to release anything. For more information, see: 
	- http://www.c2.com/cgi/wiki?UnitTest
	- there is a chapter in the PharoByExample book (http://pharobyexample.org)
	- the sunit class category
"
Class {
	#name : 'SomeClassForCompiledMethodTests',
	#superclass : 'SomeSuperClassForCompiledMethodTests',
	#instVars : [
		'x',
		'y',
		'class'
	],
	#category : 'Kernel-Extended-Tests-Methods',
	#package : 'Kernel-Extended-Tests',
	#tag : 'Methods'
}

{ #category : 'examples' }
SomeClassForCompiledMethodTests >> abstractMethod [
	"I am an abstract method"

	^ self subclassResponsibility
]

{ #category : 'examples' }
SomeClassForCompiledMethodTests >> deprecatedMethod [
	"Used to test sends of deprecation messages;
	do not recategorize in one of the 'deprecated' categories."

	self deprecated: 'example of a deprecated method'
]

{ #category : 'examples' }
SomeClassForCompiledMethodTests >> deprecatedMethod2 [
	"Used to test sends of deprecation messages;
	do not recategorize in one of the 'deprecated' categories."

	self deprecated: 'example of a deprecated method' on: 'date' in: 'someversion'
]

{ #category : 'examples' }
SomeClassForCompiledMethodTests >> deprecatedMethod3 [
	"Used to test sends of deprecation messages;
	do not recategorize in one of the 'deprecated' categories."

	self
		deprecated: 'Example of a deprecated method with transform'
		transformWith: '`@receiver deprecatedMethod3'
						-> '`@receiver deprecatedMethod3'
]

{ #category : 'examples' }
SomeClassForCompiledMethodTests >> deprecatedMethod4 [
	"Used to test sends of deprecation messages;
	do not recategorize in one of the 'deprecated' categories."

	self

		deprecated: 'Example of a deprecated method with transform'
		on: '01/01/1970'
		in: 'pharoX'
		transformWith: '`@receiver deprecatedMethod4'
						-> '`@receiver deprecatedMethod4'
]

{ #category : 'initialization' }
SomeClassForCompiledMethodTests >> initialize [ 

	<ignoreUnusedVariables: #( #x #y #class )>
	super initialize
]

{ #category : 'examples - linesofcode' }
SomeClassForCompiledMethodTests >> locComplex [

	"A multiline
	comment"

"Non-indented Comment"

	| a b |
	a := 1.
	b :=
	a + 2.

	10 timesRepeat: [
		"Comment inside a block"
		#(1 2 3) collect: [ :each |
			| c |
			c := each + b.
			c "Comment at the end of a line" ] ]
]

{ #category : 'examples - linesofcode' }
SomeClassForCompiledMethodTests >> locEmptyLineInTheEnd [
	| a |
	a := 1.
	^ a
]

{ #category : 'examples - linesofcode' }
SomeClassForCompiledMethodTests >> locEmptyLineInTheMiddle [
	| a |
	a := 1.

	^ a
]

{ #category : 'examples - linesofcode' }
SomeClassForCompiledMethodTests >> locEmptyLineWithTabInTheEnd [
	| a |
	a := 1.
	^ a
]

{ #category : 'examples - linesofcode' }
SomeClassForCompiledMethodTests >> locEmptyLineWithTabInTheMiddle [
	| a |
	a := 1.

	^ a
]

{ #category : 'examples - linesofcode' }
SomeClassForCompiledMethodTests >> locEmptyMethod [
]

{ #category : 'examples - linesofcode' }
SomeClassForCompiledMethodTests >> locEmptyMethodWithNewline [
]

{ #category : 'examples - linesofcode' }
SomeClassForCompiledMethodTests >> locEmptyMethodWithNewlineAndTab [
]

{ #category : 'examples - linesofcode' }
SomeClassForCompiledMethodTests >> locMultilineComment [
	^ 1
	"Multiline
	comment"
]

{ #category : 'examples - linesofcode' }
SomeClassForCompiledMethodTests >> locMultilineCommentWithoutWhitespace [
	^ 1
"Multiline
comment
without
whitespaces"
]

{ #category : 'examples - linesofcode' }
SomeClassForCompiledMethodTests >> locMultilineMethodComment [
	"Multiline
	comments"
	^ 1
]

{ #category : 'examples - linesofcode' }
SomeClassForCompiledMethodTests >> locMultilineMethodCommentWithoutWhitespace [
"Comment"
	^ 1
]

{ #category : 'examples - linesofcode' }
SomeClassForCompiledMethodTests >> locSimple [
	| a b c |
	a := 1.
	b := a + 2.
	c := b * 3.
	^ 1 / c
]

{ #category : 'examples - linesofcode' }
SomeClassForCompiledMethodTests >> locSingleLineComment [
	^ 1
	"Comment"
]

{ #category : 'examples - linesofcode' }
SomeClassForCompiledMethodTests >> locSingleLineCommentWithoutWhitespace [
	^ 1
"Comment"
]

{ #category : 'examples - linesofcode' }
SomeClassForCompiledMethodTests >> locSingleLineMethodComment [
	"Comment"
	^ 1
]

{ #category : 'examples - linesofcode' }
SomeClassForCompiledMethodTests >> locSingleLineMethodCommentWithoutWhitespace [
"Comment"
	^ 1
]

{ #category : 'examples' }
SomeClassForCompiledMethodTests >> methodWithPragma [

	<bebou>
	^ 1
]

{ #category : 'examples' }
SomeClassForCompiledMethodTests >> nonAbstractMethod [
	"I am not an abstract method"

	^ 4 + 5
]

{ #category : 'examples' }
SomeClassForCompiledMethodTests >> readX [
	| tmp |
	tmp := x.
	^ tmp
]

{ #category : 'examples' }
SomeClassForCompiledMethodTests >> readXandY [

	^ x + y
]

{ #category : 'examples' }
SomeClassForCompiledMethodTests >> returnPlusOne: anInteger [
	^anInteger + 1
]

{ #category : 'examples' }
SomeClassForCompiledMethodTests >> returnTrue [
	^true
]

{ #category : 'examples' }
SomeClassForCompiledMethodTests >> shouldNotImplementMethod [
	"I am not an abstract method"

	^ self shouldNotImplement
]

{ #category : 'examples' }
SomeClassForCompiledMethodTests >> writeX [

	x := 33
]

{ #category : 'examples' }
SomeClassForCompiledMethodTests >> writeXandY [

	x := 33.
	y := 66
]
