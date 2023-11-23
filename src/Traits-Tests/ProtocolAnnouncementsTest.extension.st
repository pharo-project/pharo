Extension { #name : 'ProtocolAnnouncementsTest' }

{ #category : '*Traits-Tests' }
ProtocolAnnouncementsTest >> testAddProtocolAnnouncedOnlyInTrait [

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

	self when: ProtocolAdded do: [ :ann |
		self assert: ann protocol name equals: #king.
		self assert: ann classReorganized name equals: 'TraitForTest' ].

	trait addProtocol: #king.
	self assert: numberOfAnnouncements equals: 1 ] ensure: [ self packageOrganizer removePackage: self packageNameForTests , '2' ]
]
