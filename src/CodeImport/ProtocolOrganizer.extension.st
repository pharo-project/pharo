Extension { #name : #ProtocolOrganizer }

{ #category : #'*CodeImport' }
ProtocolOrganizer >> fromSpec: aSpec [
	aSpec
		do: [ :spec |
			| name methods |
			name := spec first asSymbol.
			methods := spec allButFirst asSet.
			self addProtocol: (Protocol name: name methodSelectors: methods) ]
]

{ #category : #'*CodeImport' }
ProtocolOrganizer class >> fromSpec: aSpec [

	^ self new
		fromSpec: aSpec;
		yourself
]
