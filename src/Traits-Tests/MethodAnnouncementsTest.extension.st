Extension { #name : 'MethodAnnouncementsTest' }

{ #category : '*Traits-Tests' }
MethodAnnouncementsTest >> testCompileMethodAnnounceAdditionOnlyInTrait [

	| trait |
	[
	trait := self class classInstaller make: [ :aBuilder |
		         aBuilder
			         beTrait;
			         name: 'TraitForTest';
			         package: self packageNameForTests , '2' ].

	class := self class classInstaller make: [ :aBuilder |
		         aBuilder
			         fillFor: class;
			         traits: trait ].

	self when: MethodAdded do: [ :ann |
		self assert: ann method selector equals: #king.
		self assert: ann method protocol name equals: #titan.
		self assert: ann methodClass name equals: 'TraitForTest'.
		self assert: ann methodPackage name equals: self packageNameForTests , '2'.
		self assert: ann packagesAffected equals: { (self packageNameForTests , '2') asPackage } ].

	trait compiler
		protocol: #titan;
		install: 'king ^ 1'.

	self assert: numberOfAnnouncements equals: 1 ] ensure: [ self packageOrganizer removePackage: self packageNameForTests , '2' ]
]

{ #category : '*Traits-Tests' }
MethodAnnouncementsTest >> testRemoveMethodAnnounceRemovalOnlyInTrait [

	| trait |
	[
	trait := self class classInstaller make: [ :aBuilder |
		         aBuilder
			         beTrait;
			         name: 'TraitForTest';
			         package: self packageNameForTests , '2' ].

	class := self class classInstaller make: [ :aBuilder |
		         aBuilder
			         fillFor: class;
			         traits: trait ].

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

	self assert: numberOfAnnouncements equals: 1 ] ensure: [ self packageOrganizer removePackage: self packageNameForTests , '2' ]
]

{ #category : '*Traits-Tests' }
MethodAnnouncementsTest >> testUpdateMethodAnnounceModificationOnlyInTrait [

	| trait |
	[
	trait := self class classInstaller make: [ :aBuilder |
		         aBuilder
			         beTrait;
			         name: 'TraitForTest';
			         package: self packageNameForTests , '2' ].

	class := self class classInstaller make: [ :aBuilder |
		         aBuilder
			         fillFor: class;
			         traits: trait ].

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

	self assert: numberOfAnnouncements equals: 1 ] ensure: [ self packageOrganizer removePackage: self packageNameForTests , '2' ]
]

{ #category : '*Traits-Tests' }
MethodAnnouncementsTest >> testUpdateMethodAnnounceRecategorizationOnlyInTrait [

	| trait |
	[
	trait := self class classInstaller make: [ :aBuilder |
		         aBuilder
			         beTrait;
			         name: 'TraitForTest';
			         package: self packageNameForTests , '2' ].

	class := self class classInstaller make: [ :aBuilder |
		         aBuilder
			         fillFor: class;
			         traits: trait ].

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

	self assert: numberOfAnnouncements equals: 1 ] ensure: [ self packageOrganizer removePackage: self packageNameForTests , '2' ]
]
