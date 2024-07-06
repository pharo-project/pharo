Class {
	#name : 'ReOverrideMethod',
	#superclass : 'ReBreakingChangeReport',
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'execution' }
ReOverrideMethod >> browse [
	"This is not satisfactory because not connected to any UI element."
	SpConfirmDialog new 
		title: 'Impossible to refactor!';
		acceptLabel: 'Sure!';
		cancelLabel: 'No, forget it';
		label: ' The newly named method could override existing methods.\n 
		Do you want to browse superclasses method of ', refactoring newSelector;
		onAccept: [ StMessageBrowser browseImplementorsOf: refactoring newSelector];
		onCancel: [ ];
		openDialog
]

{ #category : 'accessing' }
ReOverrideMethod >> refactoring: aRefactoring [

	refactoring := aRefactoring
]
