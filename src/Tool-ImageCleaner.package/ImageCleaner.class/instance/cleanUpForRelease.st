api
cleanUpForRelease
	"self new cleanUpForRelease"

	Author fullName: 'Mr.Cleaner'.
	self cleanUpMethods.
	
	FreeTypeFontProvider current initialize.
	EmbeddedFreeTypeFontInstaller initialize.
	self class environment at: #MetacelloProjectRegistration ifPresent: [ :class | class resetRegistry ].
	SystemNavigation new
		allObjectsDo: [ :each | 
			((each respondsTo: #releaseCachedState) and: [ (each isKindOf: RubAbstractTextArea) not ])
				ifTrue: [ each releaseCachedState ] ].
	
	self removeEmptyCategories.
	self removeEmptyPackages.
	Smalltalk organization sortCategories.
	Smalltalk garbageCollect.
	Smalltalk cleanOutUndeclared. 
	Smalltalk fixObsoleteReferences.
	Smalltalk cleanUp: true except: #() confirming: false.	
	Author reset