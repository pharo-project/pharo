Extension { #name : #BaselineOf }

{ #category : #'*Microdown' }
BaselineOf class >> buildMicroDownUsing: aBuilder withComment: aString [
	aBuilder
		header: [ :b | b text: self name ] withLevel: 1;
		horizontalLine;
		text: 'A baseline is a kind of map to load project.';
		newLine;
		header: [ :b | b text: 'Description' ] withLevel: 3;
		text: aString;
		newLine;
		header: [ :b | b text: 'Dependencies' ] withLevel: 3.

	(self instanceSide includesLocalSelector: #baseline:) 
		ifTrue: [ aBuilder 
						codeblockTag: 'pharo'
						withBody: (self instanceSide sourceCodeAt: #baseline:) ]
]
