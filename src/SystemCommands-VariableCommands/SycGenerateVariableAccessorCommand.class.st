"
I am a command to generate accessors for given variables
"
Class {
	#name : 'SycGenerateVariableAccessorCommand',
	#superclass : 'SycRefactorVariableCommand',
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'execution' }
SycGenerateVariableAccessorCommand >> executeRefactoring [

	(ReGenerateAccessorsDriver new
		 targetClass: variables first definingClass;
		 scopes: toolContext refactoringScopes
		 scopeInstanceSide: variables first definingClass isInstanceSide
		 selectedVariables: (variables collect: [ :each | each name ])) runRefactoring
]

{ #category : 'accessing' }
SycGenerateVariableAccessorCommand >> fullBrowserOrder [

	^ 1000
]

{ #category : 'accessing' }
SycGenerateVariableAccessorCommand >> iconName [
	^ #smallAdd
]

{ #category : 'accessing' }
SycGenerateVariableAccessorCommand >> name [
	"Make sure that the user knows that this is a transformation by adding (T) in front of the menu item name."

	^ self refactoringClass menuItemString
]

{ #category : 'accessing' }
SycGenerateVariableAccessorCommand >> order [

	^ 1000
]

{ #category : 'factory method' }
SycGenerateVariableAccessorCommand >> refactoringClass [

	^ ReCreateAccessorsForVariableTransformation
]
