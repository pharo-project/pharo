"
I am a class that listens to system events and dispatch them to the right iceberg object.
I'm mainly used to mark as dirty packages whose methods and classes are modified.

I'm implemented as class side methods to avoid memory leaks and problems when unsubscribing to events.
"
Class {
	#name : #IceSystemEventListener,
	#superclass : #Object,
	#category : #'Iceberg-Changes'
}

{ #category : #'event handling' }
IceSystemEventListener class >> handleClassChange: aClassChange [

	"We should take care not only about the class package but also the package of the methods extending that class."
	self handlePackagesChange: aClassChange classAffected packages

]

{ #category : #'event handling' }
IceSystemEventListener class >> handleMethodChange: aMethodChange [

	self handlePackagesChange: aMethodChange packagesAffected
]

{ #category : #'event handling' }
IceSystemEventListener class >> handlePackageChange: aPackageChange [

	self handlePackagesChange: aPackageChange packagesAffected
]

{ #category : #'event handling' }
IceSystemEventListener class >> handlePackagesChange: packages [

	IceRepository registry do: [ :repository | | changed |
		changed := packages anySatisfy: [ :each | each isNotNil and: [ repository notifyPackageModified: each name ] ].
		changed ifTrue: [ 	Iceberg announcer announce: (IceRepositoryModified for: repository) ]]
]

{ #category : #'event handling' }
IceSystemEventListener class >> handleVersionLoaded: ann [
	"When monticello finishes to load a package, we need to verify its status to mark them as dirty or 
	 not dirty as they apply."

	| package |
	"this is ugly, but since I do not have a package I need to look for it"
	package := RPackageOrganizer default packageMatchingExtensionName: ann label.
	
	"The package can be nil".
	package ifNil: [ ^self ].

	"now we can verify its status"
	RPackageSet withoutCacheDo: [
		IceRepository registry
			do: [ :each | 
				(each includesPackageNamed: package name)
					ifTrue: [ | icePackage isDirty |
						icePackage := each packageNamed: package name.
						"Ugly hack. Should enhance for next version"
						self flag: #enhance.
						isDirty := each workingCopy referenceCommit isUnknownCommit
							ifTrue: [ true ]
							ifFalse: [ | diff |
								diff := each workingCopy referenceCommit diffToWorkingCopyForPackage: icePackage.
								diff includesPackageNamed: package name ].
						icePackage beDirty: isDirty.
						Iceberg announcer announce: (IceRepositoryModified for: each) ] ]]
]

{ #category : #'class initialization' }
IceSystemEventListener class >> initialize [

	self registerSystemAnnouncements
]

{ #category : #'class initialization' }
IceSystemEventListener class >> registerSystemAnnouncements [

	self unregisterSystemAnnouncements.
	SystemAnnouncer uniqueInstance weak
		when: ClassAnnouncement send: #handleClassChange: to: self;
		when: MethodAnnouncement send: #handleMethodChange: to: self;
		when: ClassTagAnnouncement send: #handlePackageChange: to: self;
		when: MCVersionLoaderStopped send: #handleVersionLoaded: to: self.
]

{ #category : #'class initialization' }
IceSystemEventListener class >> unregisterSystemAnnouncements [
	
	SystemAnnouncer uniqueInstance unsubscribe: self
]
