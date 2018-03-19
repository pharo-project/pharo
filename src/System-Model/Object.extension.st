Extension { #name : #Object }

{ #category : #'*System-Model' }
Object >> addModelItemsToWindowMenu: aMenu [
	"aMenu is being constructed to be presented to the user in response to the user's pressing on the menu widget in the title bar of a morphic window.  Here, the model is given the opportunity to add any model-specific items to the menu, whose default target is the SystemWindow itself."
]

{ #category : #'*System-Model' }
Object >> modelWakeUpIn: aWindow [
	"A window with me as model is being entered or expanded.  Default response is no-op" 
]
