Class {
	#name : #GLMMorphMorphicTest,
	#superclass : #GLMMorphicTest,
	#category : #'Glamour-Tests-Morphic'
}

{ #category : #tests }
GLMMorphMorphicTest >> testMorphUpdate [
	| browser number originalMorph newMorph |
	number := 42.
	browser := GLMTabulator new.
	browser row: #morph.
	browser transmit
		to: #morph;
		andShow: [ :presenter | presenter morph display: [ :x | number asString asMorph ] ].
	window := browser openOn: 42.
	originalMorph := self findLast: StringMorph in: window.
	self assert: originalMorph notNil.
	self assert: originalMorph contents equals: 42 asString.
	number := 43.
	browser update.
	newMorph := self findLast: StringMorph in: window.
	self assert: newMorph ~= originalMorph.
	self assert: newMorph notNil.
	self assert: newMorph notNil.
	self assert: newMorph contents equals: 43 asString
]

{ #category : #tests }
GLMMorphMorphicTest >> testMorphUpdateOnAnnounce [
	| browser number originalMorph newMorph announcer |
	announcer := GLMAnnouncer new.
	number := 42.
	browser := GLMTabulator new.
	browser row: #morph.
	browser transmit
		to: #morph;
		andShow: [ :presenter | 
					presenter morph
						updateOn: Announcement from: [ announcer ];
						display: [ :x | number asString asMorph ] ].
	window := browser openOn: 42.
	originalMorph := self findLast: StringMorph in: window.
	self assert: originalMorph notNil.
	self assert: originalMorph contents equals: 42 asString.
	number := 43.
	announcer announce: Announcement.
	newMorph := self findLast: StringMorph in: window.
	self assert: newMorph ~= originalMorph.
	self assert: newMorph notNil.
	self assert: newMorph notNil.
	self assert: newMorph contents equals: 43 asString
]
