Extension { #name : #ClassOrganization }

{ #category : #'*CodeExport' }
ClassOrganization >> stringForFileOut [

	^ String streamContents: [ :aStream |
		  self protocols do: [ :protocol |
			  aStream
				  nextPut: $(;
				  nextPutAll: protocol name printString.
			  protocol methodSelectors do: [ :selector |
				  aStream
					  space;
					  nextPutAll: selector ].
			  aStream
				  nextPut: $);
				  cr ] ]
]
