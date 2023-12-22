Extension { #name : 'Object' }

{ #category : '*System-Model' }
Object >> addDependent: anObject [
	"Make the given object one of the receiver's dependents."

	^ DependentsManager addDependent: anObject to: self
]

{ #category : '*System-Model' }
Object >> addModelItemsToWindowMenu: aMenu [
	"aMenu is being constructed to be presented to the user in response to the user's pressing on the menu widget in the title bar of a morphic window.  Here, the model is given the opportunity to add any model-specific items to the menu, whose default target is the SystemWindow itself."
]

{ #category : '*System-Model' }
Object >> canDiscardEdits [
	"Answer true if none of the views on this model has unaccepted edits that matter."

	^ DependentsManager canDiscardEditsTo: self
]

{ #category : '*System-Model' }
Object >> dependents [
	"Answer a collection of objects that are 'dependent' on the receiver;
	 that is, all objects that should be notified if the receiver changes."

	^ self myDependents
]

{ #category : '*System-Model' }
Object >> modelWakeUpIn: aWindow [
	"A window with me as model is being entered or expanded.  Default response is no-op"
]

{ #category : '*System-Model' }
Object >> myDependents [
	"Private. Answer a list of all the receiver's dependents."

	^ DependentsManager dependentsOf: self
]

{ #category : '*System-Model' }
Object >> myDependents: aCollectionOrNil [
	"Private. Set (or remove) the receiver's dependents list."

	DependentsManager setDependentsOf: self to: aCollectionOrNil
]

{ #category : '*System-Model' }
Object >> removeDependent: anObject [
	"Remove the given object as one of the receiver's dependents."

	^ DependentsManager removeDependent: anObject of: self
]
