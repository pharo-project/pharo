"
I'm a Announcement used to indicate a color has changed. 

Example:

(ColorSelectorDialogWindow new
                        title: 'Choose a color';
                        open;
                        announcer) on: ColorChanged  do: [:ann|
                                                                        UIManager inform: 'Selected color: ', ann newColor asString].
"
Class {
	#name : #ColorChanged,
	#superclass : #Announcement,
	#instVars : [
		'newColor'
	],
	#category : #Morphic-Widgets-ColorPicker
}

{ #category : #'instance creation' }
ColorChanged class >> to: aColor [
	^self new 
		newColor: aColor
]

{ #category : #accessing }
ColorChanged >> newColor [
	^ newColor
]

{ #category : #accessing }
ColorChanged >> newColor: anObject [
	newColor := anObject
]
