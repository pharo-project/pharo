Extension { #name : #TestCase }

{ #category : #'*Microdown' }
TestCase class >> buildMicroDownUsing: aBuilder withComment: aString [
	aBuilder
		header: [ :b | b text: self name ] withLevel: 1;
		horizontalLine;
		header: [ :b | b text: 'Description' ] withLevel: 3;
		text: aString;
		header: [ :b | b text: 'Tests' ] withLevel: 3;
		text: 'This test suite defines ';
		text: self allTestSelectors size asString;
		text: ' test methods.';
		newLine;
		text: 'Locally defined tests are: ';
		newLine.
	aBuilder
		unorderedListDuring: [ self testSelectors
				do:
					[ :each | aBuilder item: [ aBuilder monospace: self class instanceSide name , '>>#' , each ] ] ]
]
