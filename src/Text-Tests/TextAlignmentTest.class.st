"
SUnit tests for class TextAlignment
"
Class {
	#name : #TextAlignmentTest,
	#superclass : #HashAndEqualsTestCase,
	#category : #'Text-Tests-Base'
}

{ #category : #running }
TextAlignmentTest >> setUp [
	super setUp.
	prototypes add: TextAlignment centered;
		 add: TextAlignment justified;
		 add: TextAlignment leftFlush;
		 add: TextAlignment rightFlush 
]
