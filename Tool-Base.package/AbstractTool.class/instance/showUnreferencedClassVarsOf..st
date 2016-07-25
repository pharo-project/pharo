class
showUnreferencedClassVarsOf: aClass
	"Search for all class variables known to the selected class, and put up a 
	list of those that have no references anywhere in the system. The 
	search includes superclasses, so that you don't need to navigate your 
	way to the class that defines each class variable in order to determine 
	whether it is unreferenced"

	| aList aReport |
	(aClass isNil or: [ aClass isTrait ])
		ifTrue: [ ^ self ].
	aList := self systemNavigation allUnreferencedClassVariablesOf: aClass.
	aList isEmpty
		ifTrue: [ 
			^ UIManager default
				inform:
					'There are no unreferenced
class variables in
' , aClass name ].
	aReport := String
		streamContents: [ :aStream | 
			aStream
				nextPutAll: 'Unreferenced class variable(s) in ' translated;
				nextPutAll: aClass name;
				cr.
			aList
				do: [ :el | 
					aStream
						tab;
						nextPutAll: el;
						cr ] ].
	UIManager default inform: aReport