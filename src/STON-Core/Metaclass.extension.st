Extension { #name : #Metaclass }

{ #category : #'*ston-core' }
Metaclass class >> fromSton: stonReader [
	| theClassName theClass theMetaclass |
	theClassName := stonReader parseListSingleton.
	theClass := self environment at: theClassName.
	theMetaclass := theClass class.
	^ theMetaclass
]

{ #category : #'*ston-core' }
Metaclass >> stonName [
	^ #Class
]

{ #category : #'*ston-core' }
Metaclass >> stonOn: stonWriter [
	stonWriter
		writeObject: self 
		listSingleton: self theNonMetaClass name asSymbol
]
