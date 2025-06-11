Class {
	#name : 'StProfStefPlaygroundPage',
	#superclass : 'StPlaygroundPagePresenter',
	#category : 'ProfStef-Core-presenter',
	#package : 'ProfStef-Core',
	#tag : 'presenter'
}

{ #category : 'private commands' }
StProfStefPlaygroundPage class >> baseToolbarCommands [

	^ { 
		StPlaygroundNextProfStefCommand.
		StPlaygroundPreviousProfStefCommand.
		StPlaygroundChoiseLessonCommand
	}
]

{ #category : 'accessing' }
StProfStefPlaygroundPage >> windowTitle: aString [
	titleHolder := aString.
	self update.
]
