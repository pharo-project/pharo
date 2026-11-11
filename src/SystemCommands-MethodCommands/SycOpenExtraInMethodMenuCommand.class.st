Class {
	#name : 'SycOpenExtraInMethodMenuCommand',
	#superclass : 'SycOpenContextMenuCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'execution' }
SycOpenExtraInMethodMenuCommand >> activationStrategy [

	^ SycExtraMenuActivation
]

{ #category : 'context menu' }
SycOpenExtraInMethodMenuCommand >> cmCommandClass [
	^ SycMethodExtraCmCommand
]

{ #category : 'accessing' }
SycOpenExtraInMethodMenuCommand >> iconName [

	^ #forward 
]

{ #category : 'accessing' }
SycOpenExtraInMethodMenuCommand >> name [

	^ 'Extra' 
]

{ #category : 'menu' }
SycOpenExtraInMethodMenuCommand >> pureMenuCommands [
	"The commands of the class of this opener that apply here, in the order they are built."
	^ self classFamilyPureMenuCommands
]
