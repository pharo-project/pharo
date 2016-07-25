private
buildString

	| first  separator |
	separator := ' > '.
	first := self model selectedPackage.
	first
		ifNil: [ ^''].			
	^ String streamContents: [ :str | 
			str nextPutAll: first name.
			self model selectedClass
				ifNotNil: [ :class |
					str nextPutAll:  separator ; nextPutAll: class name.
					self model selectedCategory 
						ifNotNil: [ :category |
							str nextPutAll:  separator ; nextPutAll: category.
							self model selectedMethod
								ifNotNil: [ :method | str nextPutAll: separator ; nextPutAll: method selector printString]]].
			str].
