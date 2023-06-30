Class {
	#name : #RBOverrideMethod,
	#superclass : #RBBreakingChangeReport,
	#category : #'Refactoring-UI-Drivers'
}

{ #category : #execution }
RBOverrideMethod >> execute [
	"This is not satisfactory because not connected to any UI element."
	SpConfirmDialog new 
		title: 'Impossible to refactor!';
		acceptLabel: 'Sure!';
		cancelLabel: 'No, forget it';
		label: ' The newly named method could override existing methods.\n 
		Do you want to browse superclasses method of ', refactoring newSelector;
		onAccept: [ MessageBrowser browseImplementorsOf: refactoring newSelector];
		onCancel: [ ];
		openDialog
]

{ #category : #accessing }
RBOverrideMethod >> refactoring: aRefactoring [

	refactoring := aRefactoring
]
