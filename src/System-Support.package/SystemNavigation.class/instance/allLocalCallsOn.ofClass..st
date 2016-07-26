query
allLocalCallsOn: aSymbol ofClass: aClass
	"Answer a SortedCollection of all the methods that call on aSymbol, anywhere in my class hierarchy."
	
	| aSet cls |
	aSet := Set new.
	cls := aClass theNonMetaClass.
	cls withAllSuperAndSubclassesDo: [ :class |
		(class whichSelectorsReferTo: aSymbol)
			do: [:sel |
				aSet add: (self createMethodNamed: sel realParent: class) ]].
		
	cls class withAllSuperAndSubclassesDo: [ :class |
		(class whichSelectorsReferTo: aSymbol)
			do: [:sel |
				aSet add: (self createMethodNamed: sel realParent: class) ]].
	^aSet