"
Requests to save a textual version of the selected class to a new file in chunk format and .st extension. Instance and class side methods are both saved
"
Class {
	#name : 'SycFileOutMethodCommand',
	#superclass : 'SycMethodExtraCmCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'accessing' }
SycFileOutMethodCommand >> description [

	^ 'Requests to save a textual version of the selected class to a new file in chunk format and .st extension. Instance and class side methods are both saved'
]

{ #category : 'executing' }
SycFileOutMethodCommand >> executeRefactoring [

	| filename |
	filename := self methods size = 1
		            ifTrue: [ self methods first name ]
		            ifFalse: [ 'methodsfileout' ].

	^ Fileouter
		fileoutNamed: filename
		cofigureWith: [ :fileouter | 
			self methods do: [ :each | fileouter addMethod: each ] ]
]

{ #category : 'accessing' }
SycFileOutMethodCommand >> icon [

	^ self iconNamed: #save
]

{ #category : 'accessing' }
SycFileOutMethodCommand >> name [

	^ 'File out'
]
