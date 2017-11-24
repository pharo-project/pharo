"
An URLPlugin is a plugin which shows the URL of the current navigation
"
Class {
	#name : #URLPlugin,
	#superclass : #InformationLinePlugin,
	#category : #'NautilusCommon-Plugin'
}

{ #category : #position }
URLPlugin class >> defaultPosition [

	^ #middle
]

{ #category : #information }
URLPlugin class >> description [

	^ 'Display the URI of the selected element'
]

{ #category : #private }
URLPlugin >> buildString [

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

]
