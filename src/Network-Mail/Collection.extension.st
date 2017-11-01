Extension { #name : #Collection }

{ #category : #'*Network-Mail' }
Collection >> asEmailHeaderString [

	^ String streamContents: [ :str | 
			self
				do: [ :e | str nextPutAll: e asEmailHeaderString ]
				separatedBy: [ str nextPutAll: ', ' ] ]
]
