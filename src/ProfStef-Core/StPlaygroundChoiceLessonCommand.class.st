Class {
	#name : 'StPlaygroundChoiceLessonCommand',
	#superclass : 'StPlaygroundCommand',
	#category : 'ProfStef-Core-command',
	#package : 'ProfStef-Core',
	#tag : 'command'
}

{ #category : 'default' }
StPlaygroundChoiceLessonCommand class >> defaultDescription [ 
	^ 'go to a selected lessons'.
]

{ #category : 'accessing' }
StPlaygroundChoiceLessonCommand class >> defaultIconName [

	^ #glamorousTable
]

{ #category : 'default' }
StPlaygroundChoiceLessonCommand class >> defaultName [
	^ 'go to a lessons'.
]

{ #category : 'executing' }
StPlaygroundChoiceLessonCommand >> execute [
	(StProfStefChoiceLesson profInst: (ProfStef default) model: context) open.
]
