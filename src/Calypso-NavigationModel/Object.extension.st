Extension { #name : #Object }

{ #category : #'*Calypso-NavigationModel' }
Object >> adoptForNewCalypsoQuery [

	^self
]

{ #category : #'*Calypso-NavigationModel' }
Object >> asCalypsoBrowserItem [

	^self calypsoEnvironmentType convertToCalypsoBrowserItem: self 
]

{ #category : #'*Calypso-NavigationModel' }
Object >> calypsoEnvironmentType [

	^self class
]

{ #category : #'*Calypso-NavigationModel' }
Object class >> checkCalypsoItem: anObject isEqualTo: anObject2 [
	^anObject = anObject2
]

{ #category : #'*Calypso-NavigationModel' }
Object class >> checkCalypsoItem: anObject isSameAs: anObject2 [
	^anObject == anObject2
]

{ #category : #'*Calypso-NavigationModel' }
Object class >> decorateBrowserItem: aBrowserItem by: anEnvironmentPlugin [
	^ anEnvironmentPlugin
		decorateBrowserItem: aBrowserItem
		ofObject: aBrowserItem actualObject
]

{ #category : #'*Calypso-NavigationModel' }
Object class >> isCalypsoItemType: aClass [
	^self includesBehavior: aClass
]

{ #category : #'*Calypso-NavigationModel' }
Object class >> itemsSortOrderForCalypso [
	"This method allows specify order of group of concrete type of items 
	Larger value will put items to the end of list comparing to the items of another type.
	By default it is big value multiplied by hash of type. 
	It gives behavior where items of different type will be always splitted in the list
	and sorted separatelly"
	^1000 * self hash
]

{ #category : #'*Calypso-NavigationModel' }
Object class >> prepareCalypsoItemCopy: aBrowserItem [
]

{ #category : #'*Calypso-NavigationModel' }
Object >> printAsConstantQueryItem [
	^self printString
]

{ #category : #'*Calypso-NavigationModel' }
Object >> printSystemPath [

	^self calypsoEnvironmentType printSystemPathTo: self 
]

{ #category : #'*Calypso-NavigationModel' }
Object class >> printSystemPathTo: myInstance [
	^''
]
