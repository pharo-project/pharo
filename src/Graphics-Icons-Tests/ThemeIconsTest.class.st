"
I test `ThemeIcons`, the icon packs that widgets take their icons from.
"
Class {
	#name : 'ThemeIconsTest',
	#superclass : 'TestCase',
	#category : 'Graphics-Icons-Tests',
	#package : 'Graphics-Icons-Tests'
}

{ #category : 'tests' }
ThemeIconsTest >> testEmptyPackAnswersBlankIcons [

	self assert: (ThemeIcons empty iconNamed: #add) extent equals: 0 @ 0
]

{ #category : 'tests' }
ThemeIconsTest >> testEmptyPackHasNoIcons [

	self deny: ThemeIcons empty hasIcons
]
