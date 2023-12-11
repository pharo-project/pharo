Class {
	#name : 'ShClassInstallerAnnouncementsTest',
	#superclass : 'ShClassInstallerTest',
	#instVars : [
		'numberOfAnnouncements',
		'newTrait'
	],
	#category : 'Shift-ClassBuilder-Tests-Installer',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'Installer'
}

{ #category : 'running' }
ShClassInstallerAnnouncementsTest >> setUp [

	super setUp.
	numberOfAnnouncements := 0
]

{ #category : 'running' }
ShClassInstallerAnnouncementsTest >> tearDown [

	SystemAnnouncer uniqueInstance unsubscribe: self.
	super tearDown
]

{ #category : 'tests' }
ShClassInstallerAnnouncementsTest >> testClassRepackagingShouldAnnounceClassModified [

	| aClass |
	aClass := ShiftClassInstaller make: [ :builder |
		          builder
			          name: #SHClass;
			          package: self generatedClassesPackageName ].

	self when: ClassRepackaged do: [ :ann |
		self assert: ann oldPackage name equals: self generatedClassesPackageName.
		self assert: ann newPackage name equals: self generatedClassesPackageName , '2'.
		self assert: ann classRepackaged name equals: #SHClass ].

	[
	ShiftClassInstaller 
		update: aClass 
		to: [ :builder | builder package: self generatedClassesPackageName , '2' ].

	self assert: numberOfAnnouncements equals: 1 ] ensure: [ self packageOrganizer removePackage: self generatedClassesPackageName , '2' ]
]

{ #category : 'tests' }
ShClassInstallerAnnouncementsTest >> testRecompilingClassUsingTraitsDoesNotAnnounceProtocolChange [
	"Regression test because recompiling a class with trait was announcing protocol creations."

	newTrait := ShiftClassInstaller make: [ :builder |
		            builder
			            name: #TSHTrait;
			            package: self generatedClassesPackageName;
			            beTrait ].

	self when: ProtocolAnnouncement do: [ :ann | self fail: 'Recreating a class should not announce anything.' ].
	self when: MethodAnnouncement do: [ :ann | self fail: 'Recreating a class should not announce anything.' ].

	newClass := ShiftClassInstaller make: [ :builder |
		            builder
			            name: #SHClassWithTrait;
			            superclass: Object;
			            traitComposition: newTrait;
			            package: self generatedClassesPackageName ]
]

{ #category : 'protocol' }
ShClassInstallerAnnouncementsTest >> when: anAnnouncement do: aBlock [

	SystemAnnouncer uniqueInstance
		when: anAnnouncement
		do: [ :ann |
			numberOfAnnouncements := numberOfAnnouncements + 1.
			aBlock cull: ann ]
		for: self
]
