Class {
	#name : 'StProfStefPlaygroundPage',
	#superclass : 'StPlaygroundPagePresenter',
	#category : 'ProfStef-Core-Presenter',
	#package : 'ProfStef-Core',
	#tag : 'Presenter'
}

{ #category : 'private commands' }
StProfStefPlaygroundPage class >> baseToolbarCommands [

	^ { 
		StPlaygroundNextProfStefCommand.
		StPlaygroundPreviousProfStefCommand.
		StPlaygroundChoiceLessonCommand
	}
]

{ #category : 'accessing' }
StProfStefPlaygroundPage >> windowTitle: aString [
	titleHolder := aString.
	self update.
]
