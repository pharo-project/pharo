Class {
	#name : 'SycOpenExtraInClassMenuCommand',
	#superclass : 'SycOpenContextMenuCommand',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'activation' }
SycOpenExtraInClassMenuCommand class >> browserContextMenuActivation [
	<classAnnotation>

	^ ClyContextMenuActivation 
		byRootGroupItemOrder: 10200 
		for: ClyClass asCalypsoItemContext
]

{ #category : 'execution' }
SycOpenExtraInClassMenuCommand >> activationStrategy [
	^ SycExtraMenuActivation
]

{ #category : 'context menu' }
SycOpenExtraInClassMenuCommand >> cmCommandClass [
	^ SycClassExtraCmCommand
]

{ #category : 'accessing' }
SycOpenExtraInClassMenuCommand >> iconName [
	^ #smallRedo
]

{ #category : 'accessing' }
SycOpenExtraInClassMenuCommand >> name [

	^ 'Extras'
]

{ #category : 'menu' }
SycOpenExtraInClassMenuCommand >> pureMenuCommands [
	"The commands of the class of this opener that apply here, in the order they are built."
	^ self classFamilyPureMenuCommands
]
