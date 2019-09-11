"
SUnit tests for AbstractTutorial
"
Class {
	#name : #AbstractTutorialTest,
	#superclass : #TestCase,
	#category : #'ProfStef-Tests-Tests'
}

{ #category : #tests }
AbstractTutorialTest >> testLessonsReturnsAllLessonInstances [
	| lessons |
	lessons := MockTutorial new lessons.
	self assert: 3 equals:lessons size.
	self assert: 'first' equals: lessons first title.
	self assert: 'second' equals: (lessons at:2 ) title.
	self assert: 'third' equals: lessons last title
]

{ #category : #tests }
AbstractTutorialTest >> testTitleHumanizeClassName [
	self assert: 'How To Make Your Own Tutorial' equals: HowToMakeYourOwnTutorial title.
	self assert: 'Pharo Syntax Tutorial' equals: PharoSyntaxTutorial title.
]

{ #category : #tests }
AbstractTutorialTest >> testTutorialRaisesShouldBeImplemented [
	| tutorial |
	tutorial := AbstractTutorial new.
	self 
		should: [tutorial tutorial]
		raise: Error
		withExceptionDo: [:anException | 
			self 
				assert:  ShouldBeImplemented 
				equals: anException class
		].
	
]

{ #category : #tests }
AbstractTutorialTest >> testTutorialsReturnsAllButMockTutorial [
	| tutorials |
	tutorials := AbstractTutorial tutorials.
	self assert: (tutorials includes: PharoSyntaxTutorial).
	self assert: (tutorials includes: HowToMakeYourOwnTutorial).
	self deny: (tutorials includes: MockTutorial).
]
