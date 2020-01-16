Class {
	#name : #EFInternalTest,
	#superclass : #TestCase,
	#category : #'EnlumineurFormatter-Tests'
}

{ #category : #tests }
EFInternalTest >> testCurrentLineLength [
	
	| formatter |
	formatter := EFFormatter new.
	formatter writeString: 'abdef'.
	self assert: formatter currentLineLength equals: 'abdef' size.
	formatter newLines: 2.
	self assert: formatter currentLineLength equals: 0.
]

{ #category : #tests }
EFInternalTest >> testCurrentLineLengthWithStringContainingCR [
	
	| formatter |
	formatter := EFFormatter new.
	formatter writeString: 'abdef', Character cr asString, 'ef'.
	self assert: formatter currentLineLength equals: 'ef' size.

]

{ #category : #tests }
EFInternalTest >> testLineStartIsCorrectAfterIndent [
	
	| formatter |
	formatter := EFFormatter new.
	formatter numberOfSpacesInIndent: 5.
	formatter indent: 4.
	self assert: formatter indentString equals:   '	'.	
	formatter newLines: 3.
	self assert: formatter lineStart equals: 3
	"indeed the lineStart in the beginning of the last line."

]

{ #category : #tests }
EFInternalTest >> testLineStartIsCorrectAfterNewLines [
	
	| formatter |
	formatter := EFFormatter new.
	formatter newLines: 3.
	self assert: formatter lineStart equals: 3
	
]

{ #category : #tests }
EFInternalTest >> testLineStartIsInitializedToZero [
	
	| formatter |
	formatter := EFFormatter new.
	self assert: formatter lineStart equals: 0.
	
]

{ #category : #tests }
EFInternalTest >> testLineStartIsOkAfterWriteString [
	
	| formatter |
	formatter := EFFormatter new.
	self assert: formatter lineStart equals: 0.
	formatter writeString: 'abdef'.
	self assert: formatter lineStart equals: 0.
]

{ #category : #tests }
EFInternalTest >> testLineStartStringContainingCR [
	
	| formatter |
	formatter := EFFormatter new.
	formatter writeString: 'abdef', Character cr asString, 'ef'.
	self assert: formatter lineStart equals: 'abdef' size + 1.

]
