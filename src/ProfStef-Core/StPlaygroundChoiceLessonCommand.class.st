Class {
	#name : 'StPlaygroundChoiceLessonCommand',
	#superclass : 'StPlaygroundCommand',
	#category : 'ProfStef-Core-Command',
	#package : 'ProfStef-Core',
	#tag : 'Command'
}

{ #category : 'default' }
StPlaygroundChoiceLessonCommand class >> defaultDescription [ 
	^ 'go to a selected lesson'.
]

{ #category : 'accessing' }
StPlaygroundChoiceLessonCommand class >> defaultIconName [

	^ #table
]

{ #category : 'default' }
StPlaygroundChoiceLessonCommand class >> defaultName [
	^ 'go to a lesson'.
]

{ #category : 'executing' }
StPlaygroundChoiceLessonCommand >> execute [
	(StProfStefLessonChooser model: context) open.
]
