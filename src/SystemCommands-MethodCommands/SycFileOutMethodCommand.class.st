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
SycFileOutMethodCommand >> defaultFileName [
	| method |
	^ self methods size = 1
		ifTrue: [
			method := self methods first.
			method methodClass name , '-' , method selector ]
		ifFalse: [ 'methodsfileout' ]
]

{ #category : 'accessing' }
SycFileOutMethodCommand >> description [

	^ 'Requests to save a textual version of the selected class to a new file in chunk format and .st extension. Instance and class side methods are both saved'
]

{ #category : 'executing' }
SycFileOutMethodCommand >> executeRefactoring [
	^ Fileouter
		fileoutNamed: self defaultFileName
		configureWith: [ :fileouter | 
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
