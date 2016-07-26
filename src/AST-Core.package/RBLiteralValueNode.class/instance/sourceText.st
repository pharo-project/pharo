accessing
sourceText
	^ sourceText ifNil: [
		String streamContents: [:aStream|			
			value isSymbol
				ifTrue: [
					aStream nextPut: $#.
					((RBScanner isSelector: value) and: [value ~~ #'||']) 
						ifTrue: [aStream nextPutAll: value]
						ifFalse: [value asString printOn: aStream]]
				ifFalse: [ 
					value class == Character 
						ifTrue: 
							[aStream nextPut: $$;	nextPut: value]
						ifFalse: [ value storeOn: aStream ]]] ]