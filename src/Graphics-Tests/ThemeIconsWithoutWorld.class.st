"
I am theme icons as they are while Pharo loads, before Morphic's initialization installs a world: my current world is nil. I hold the icons of another ThemeIcons.
"
Class {
	#name : 'ThemeIconsWithoutWorld',
	#superclass : 'ThemeIcons',
	#category : 'Graphics-Tests',
	#package : 'Graphics-Tests'
}

{ #category : 'instance creation' }
ThemeIconsWithoutWorld class >> from: aThemeIcons [

	^ self basicNew initializeFrom: aThemeIcons
]

{ #category : 'accessing' }
ThemeIconsWithoutWorld >> currentWorld [
	"No world is installed yet"

	^ nil
]

{ #category : 'initialization' }
ThemeIconsWithoutWorld >> initializeFrom: aThemeIcons [
	"Hold the icons of aThemeIcons, with a cache of my own"

	self copyFrom: aThemeIcons.
	formSetsCache := nil
]
