"
I am a test case specialized in checking that the announcements linked to protocol management are right.
"
Class {
	#name : 'TraitMethodAnnouncementsTest',
	#superclass : 'AbstractClassDescriptionAnnouncementTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'running' }
TraitMethodAnnouncementsTest >> tearDown [
	"We need to do the tear down before to ensure we are already unsubscribed of the announcements."

	<tearDownIsNotLast>
	super tearDown.
	self packageOrganizer removePackage: self packageNameForTests , '2'
]

{ #category : 'tests' }
TraitMethodAnnouncementsTest >> testCompileMethodAnnounceAdditionOnlyInTrait [

	| trait |
	trait := self class classInstaller make: [ :aBuilder |
			         aBuilder
				         beTrait;
				         name: 'TraitForTest';
				         package: self packageNameForTests , '2' ].

	class := class classInstaller update: class to: [ :aBuilder | aBuilder traits: trait ].

	self when: MethodAdded do: [ :ann |
			self assert: ann method selector equals: #king.
			self assert: ann method protocol name equals: #titan.
			self assert: ann methodClass name equals: 'TraitForTest'.
			self assert: ann methodPackage name equals: self packageNameForTests , '2'.
			self assert: ann packagesAffected equals: { (self packageNameForTests , '2') asPackage } ].

	trait compiler
		protocol: #titan;
		install: 'king ^ 1'.

	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'tests' }
TraitMethodAnnouncementsTest >> testRecompilingAClassShouldNotAnnounceAnything [
	"At some point, recompiling a class was announcing a lot of unneded announcements. I am here to ensure they are not announced anymore. "

	| trait |
	trait := self class classInstaller make: [ :aBuilder |
			         aBuilder
				         beTrait;
				         name: 'TraitForTest';
				         package: self packageNameForTests , '2' ].

	class := class classInstaller update: class to: [ :aBuilder | aBuilder traits: trait ].

	trait compiler
		protocol: #titan;
		install: 'king ^ 1'.

	"We should probably add more announcements here."
	self when: ProtocolAdded do: [ :ann | self fail: 'There should be no announcement here.' ].
	self when: ProtocolRemoved do: [ :ann | self fail: 'There should be no announcement here.' ].
	self when: MethodRecategorized do: [ :ann | self fail: 'There should be no announcement here.' ].
	self when: ClassAdded do: [ :ann | self fail: 'There should be no announcement here.' ].
	self when: ClassRepackaged do: [ :ann | self fail: 'There should be no announcement here.' ].

	trait addSlot: #slot asSlot.

	self assert: numberOfAnnouncements equals: 0
]

{ #category : 'tests' }
TraitMethodAnnouncementsTest >> testRemoveMethodAnnounceRemovalOnlyInTrait [

	| trait |
	trait := self class classInstaller make: [ :aBuilder |
			         aBuilder
				         beTrait;
				         name: 'TraitForTest';
				         package: self packageNameForTests , '2' ].

	class := class classInstaller update: class to: [ :aBuilder | aBuilder traits: trait ].

	trait compiler
		protocol: #titan;
		install: 'king ^ 1'.

	self when: MethodRemoved do: [ :ann |
			self assert: ann method selector equals: #king.
			self assert: ann method protocol name equals: #titan.
			self assert: ann methodClass name equals: 'TraitForTest'.
			self assert: ann methodPackage name equals: self packageNameForTests , '2'.
			self assert: ann packagesAffected equals: { (self packageNameForTests , '2') asPackage } ].

	trait removeSelector: #king.

	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'tests' }
TraitMethodAnnouncementsTest >> testUpdateMethodAnnounceModificationOnlyInTrait [

	| trait |
	trait := self class classInstaller make: [ :aBuilder |
			         aBuilder
				         beTrait;
				         name: 'TraitForTest';
				         package: self packageNameForTests , '2' ].

	class := class classInstaller update: class to: [ :aBuilder | aBuilder traits: trait ].

	trait compiler
		protocol: #titan;
		install: 'king ^ 2'.

	self when: MethodModified do: [ :ann |
			self assert: ann method selector equals: #king.
			self assert: ann method protocol name equals: #titan.
			self assert: ann methodClass name equals: 'TraitForTest'.
			self assert: ann methodPackage name equals: self packageNameForTests , '2'.
			self assert: ann packagesAffected equals: { (self packageNameForTests , '2') asPackage } ].

	trait compiler
		protocol: #titan;
		install: 'king ^ 1'.

	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'tests' }
TraitMethodAnnouncementsTest >> testUpdateMethodAnnounceRecategorizationOnlyInTrait [

	| trait |
	trait := self class classInstaller make: [ :aBuilder |
			         aBuilder
				         beTrait;
				         name: 'TraitForTest';
				         package: self packageNameForTests , '2' ].

	class := class classInstaller update: class to: [ :aBuilder | aBuilder traits: trait ].

	trait compiler
		protocol: #demon;
		install: 'king ^ 2'.

	self when: MethodRecategorized do: [ :ann |
			self assert: ann method selector equals: #king.
			self assert: ann oldProtocol name equals: #demon.
			self assert: ann newProtocol name equals: #titan.
			self assert: ann methodClass name equals: 'TraitForTest'.
			self assert: ann methodPackage name equals: self packageNameForTests , '2'.
			self assert: ann packagesAffected equals: { (self packageNameForTests , '2') asPackage } ].

	trait compiler
		protocol: #titan;
		install: 'king ^ 1'.

	self assert: numberOfAnnouncements equals: 1
]
