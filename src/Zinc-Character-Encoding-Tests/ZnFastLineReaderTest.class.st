Class {
	#name : #ZnFastLineReaderTest,
	#superclass : #TestCase,
	#category : #'Zinc-Character-Encoding-Tests'
}

{ #category : #testing }
ZnFastLineReaderTest >> testLinesDo [
	| lines reader |
	lines := #( 'foo' 'bar' 'last').
	reader := ZnFastLineReader on: (Character lf join: lines) readStream.
	self 
		assert: (Array streamContents: [ :out | 
					reader linesDo: [ :line | out nextPut: line ] ])
		equals: lines
]

{ #category : #testing }
ZnFastLineReaderTest >> testNextLine [
	| reader |
	reader := ZnFastLineReader on: 'abc' readStream.
	self assert: reader nextLine equals: 'abc'.
	self assert: reader atEnd.
	self assert: reader nextLine  equals: nil.
	reader := ZnFastLineReader on: '' readStream.
	self assert: reader nextLine equals: nil.
	self assert: reader atEnd.
	reader := ZnFastLineReader on: (String withAll: { 
		$a. Character cr. 
		$b. Character lf. 
		$c }) readStream.
	self assert: reader nextLine equals: 'a'.
	self assert: reader nextLine equals: 'b'.
	self assert: reader nextLine equals: 'c'.
	self assert: reader nextLine equals: nil.
	reader := ZnFastLineReader on: (String withAll: { 
		$a. Character cr. Character lf.  
		$b. Character cr. Character lf. 
		$c. Character cr. Character lf }) readStream.
	self assert: reader nextLine equals: 'a'.
	self assert: reader nextLine equals: 'b'.
	self assert: reader nextLine equals: 'c'.
	self assert: reader nextLine equals: nil.
	reader := ZnFastLineReader on: (String withAll: { 
		$a. Character cr. Character lf.  
		Character cr. Character lf. 
		$c. Character cr. Character lf }) readStream.
	self assert: reader nextLine equals: 'a'.
	self assert: reader nextLine equals: ''.
	self assert: reader nextLine equals: 'c'.
	self assert: reader atEnd
]
