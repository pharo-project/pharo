Class {
	#name : 'ClassAnnouncementsTest',
	#superclass : 'AbstractClassDescriptionAnnouncementTest',
	#category : 'Kernel-Tests-Classes',
	#package : 'Kernel-Tests',
	#tag : 'Classes'
}

{ #category : 'tests' }
ClassAnnouncementsTest >> testRemovingAClassWithExtension [
	"Regression test about an issue where class removed was not returning the extension packages in its affected packages"

	class compile: 'billy ^ #meow' classified: '*' , self packageNameForTests , '2'.

	self when: ClassRemoved do: [ :ann |
		self assert: ann classAffected originalName equals: self classNameForTests.
		self assert: ann packageAffected name equals: self packageNameForTests.
		self assertCollection: (ann packagesAffected collect: [ :package | package name ]) hasSameElements: {
				self packageNameForTests.
				(self packageNameForTests , '2') } ].

	class removeFromSystem.

	self assert: numberOfAnnouncements equals: 1
]
