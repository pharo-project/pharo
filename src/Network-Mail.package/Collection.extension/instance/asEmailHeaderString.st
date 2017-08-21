*Network-Mail
asEmailHeaderString

	^ String streamContents: [ :str | 
			self
				do: [ :e | str nextPutAll: e asEmailHeaderString ]
				separatedBy: [ str nextPutAll: ', ' ] ]