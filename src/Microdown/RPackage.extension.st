Extension { #name : #RPackage }

{ #category : #'*Microdown' }
RPackage >> buildMicroDownUsing: aBuilder withComment: aString [

	self class environment
		at: self name
		ifPresent: [ :cls | "there is a class with the package name. "
			(cls name beginsWith: 'BaselineOf') 
				ifTrue: [ 
					aBuilder
						header: [ :b | b text: self name ] withLevel: 1;
						horizontalLine;
						text: 'A baseline is a kind of map to load project.';
						newLine;
						header: [ :b | b text: 'Description' ] withLevel: 3;
						text: aString;
						newLine;
						header: [ :b | b text: 'Dependencies' ] withLevel: 3;
						codeblockTag: 'pharo' 
						withBody: (cls
							 sourceCodeAt: #baseline:
							 ifAbsent: [ 'No baseline! Houston we have a problem' ]).
					^ self ]].

	 self buildPackageComment: aString using: aBuilder 
]

{ #category : #'*Microdown' }
RPackage >> buildPackageComment: aString using: aBuilder [

	^ aBuilder
		  header: [ :b | 
			  b
				  text: 'Package: ';
				  text: self name ]
		  withLevel: 1;
		  horizontalLine;
		  text: aString
]
