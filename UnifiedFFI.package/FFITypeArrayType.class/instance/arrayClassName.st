private
arrayClassName
	^ self objectClass isAnonymous 
			ifTrue: [ self annonymousClassCreator ]
			ifFalse: [ self objectClass name ]