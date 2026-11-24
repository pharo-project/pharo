"
I check theme icons when no world is installed yet, as while Pharo loads.
"
Class {
	#name : 'ThemeIconsTest',
	#superclass : 'TestCase',
	#category : 'Graphics-Tests',
	#package : 'Graphics-Tests'
}

{ #category : 'tests' }
ThemeIconsTest >> testIconsWithoutAWorldAreAtScaleOne [

	| icons name |
	icons := ThemeIconsWithoutWorld from: ThemeIcons current.
	name := icons allIconNames anyOne.

	self assert: icons displayScaleFactor equals: 1.
	self assert: (icons iconFormSetNamed: name) extent equals: (icons icons at: name) extent * (1 / icons scale)
]
