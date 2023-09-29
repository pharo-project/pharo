Class {
	#name : 'ShClassInstallerAnnouncementsTest',
	#superclass : 'ShClassInstallerTest',
	#instVars : [
		'numberOfAnnouncements',
		'newTrait'
	],
	#category : 'Shift-ClassInstaller-Tests',
	#package : 'Shift-ClassInstaller-Tests'
}

{ #category : 'running' }
ShClassInstallerAnnouncementsTest >> setUp [

	super setUp.
	numberOfAnnouncements := 0
]

{ #category : 'running' }
ShClassInstallerAnnouncementsTest >> tearDown [

	SystemAnnouncer uniqueInstance unsubscribe: self.
	{ newTrait } do: [ :class | class ifNotNil: [ class removeFromSystem ] ].
	super tearDown
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
