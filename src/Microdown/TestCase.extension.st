Extension { #name : #TestCase }

{ #category : #'*Microdown' }
TestCase class >> buildMicroDownUsing: aBuilder withComment: aString [
	
	| number |
	number := self allTestSelectors size.
	aBuilder
		header: [ aBuilder text: self name ] withLevel: 1;
		horizontalLine;
		header: [ aBuilder text: 'Description' ] withLevel: 3;
		text: aString;
		newLine;
		header: [ aBuilder text: 'Tests' ] withLevel: 3;
		text: 'This test suite '.
	number isZero
		ifTrue: [ ^ aBuilder text: 'has no test methods.'].
	
	aBuilder 
		text: 'defines ', number asString, ' test method'.
	number = 1
		ifFalse: [ aBuilder text: 's'  ]. 
	aBuilder text: ':'.

	aBuilder 	
		newLine;
		unorderedListDuring: [ self testSelectors
				do: [ :each | aBuilder item: [ aBuilder monospace: (self class instanceSide name , '>>#' , each) asString ] ]]
]
