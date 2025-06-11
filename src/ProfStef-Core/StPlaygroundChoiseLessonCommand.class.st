Class {
	#name : 'StPlaygroundChoiseLessonCommand',
	#superclass : 'StPlaygroundCommand',
	#category : 'ProfStef-Core-Command',
	#package : 'ProfStef-Core',
	#tag : 'Command'
}

{ #category : 'default' }
StPlaygroundChoiseLessonCommand class >> defaultDescription [ 
	^ 'hello world'.
]

{ #category : 'accessing' }
StPlaygroundChoiseLessonCommand class >> defaultIconName [

	^ #glamorousTable
]

{ #category : 'default' }
StPlaygroundChoiseLessonCommand class >> defaultName [
	^ 'go to a lessons'.
]

{ #category : 'executing' }
StPlaygroundChoiseLessonCommand >> execute [
	(StProfStefChoiseLesson profInst: (ProfStef default) model: context) open.
]
