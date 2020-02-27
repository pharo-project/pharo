"
Provides optimized model support
"
Class {
	#name : #Model,
	#superclass : #Object,
	#instVars : [
		'dependents',
		'announcer'
	],
	#category : #'System-Model-Base'
}

{ #category : #accessing }
Model class >> theme [
	^ Smalltalk ui theme
]

{ #category : #'dependents-private' }
Model >> actAsExecutor [
	"Prepare the receiver to act as executor for any resources associated with it"
	self breakDependents
]

{ #category : #dependents }
Model >> addDependent: anObject [
	"Make the given object one of the receiver's dependents."

	(self dependents includes: anObject) ifFalse:
		[self myDependents: (self dependents copyWithDependent: anObject)].
	
	^ anObject
]

{ #category : #window }
Model >> addModelItemsToWindowMenu: aMenu [
	"aMenu is being constructed to be presented to the user in response to the user's pressing on the menu widget in the title bar of a morphic window.  Here, the model is given the opportunity to add any model-specific items to the menu, whose default target is the SystemWindow itself."
]

{ #category : #accessing }
Model >> announcer [

	^ announcer ifNil: [ announcer := Announcer new ]
]

{ #category : #backstops }
Model >> arrowKey: aChar from: view [
	"backstop; all the PluggableList* classes actually handle arrow keys, and the models handle other keys."
	^false
]

{ #category : #edits }
Model >> canDiscardEdits [
	"Answer true if none of the views on this model has unaccepted edits that matter."

	dependents ifNil: [ ^ true ].
	
	self dependents
		do: [:each | each canDiscardEdits ifFalse: [ ^ false ] ]
		without: self.
		
	^ true
]

{ #category : #updating }
Model >> changed [
	"Receiver changed in a general way; inform all the dependents by 
	sending each dependent an update: message."

	self changed: self
]

{ #category : #updating }
Model >> changed: aParameter [ 
	"Receiver changed. The change is denoted by the argument aParameter. 
	Usually the argument is a Symbol that is part of the dependent's change 
	protocol. Inform all of the dependents."

	self dependents do: [:aDependent | aDependent update: aParameter]
]

{ #category : #updating }
Model >> changed: anAspect with: anObject [
	"Receiver changed. The change is denoted by the argument anAspect. 
	Usually the argument is a Symbol that is part of the dependent's change 
	protocol. Inform all of the dependents. Also pass anObject for additional information."

	self dependents do: [:aDependent | aDependent update: anAspect with: anObject]
]

{ #category : #dependents }
Model >> dependents [
	"Answer a collection of objects that are 'dependent' on the receiver;
	 that is, all objects that should be notified if the receiver changes."

	^ self myDependents ifNil: [#()]
]

{ #category : #announcing }
Model >> destroyAnnouncer [

	announcer := nil.
]

{ #category : #edits }
Model >> hasUnacceptedEdits [
	"Answer true if any of the views on this model has unaccepted edits."

	dependents ifNil: [^ false].
	self dependents
		do: [:each | 
			self flag: #pharoFixMe. "The check for #respondsTo: is a hack necessary just because the old
			Browser uses it wrong. We need to change this after old Browser removal"
			((each respondsTo: #hasUnacceptedEdits) and: [ each hasUnacceptedEdits ]) 
				ifTrue: [^ true]]
		without: self.
	^ false

]

{ #category : #window }
Model >> modelWakeUpIn: aWindow [
	"A window with me as model is being entered or expanded.  Default response is no-op" 
]

{ #category : #'dependents-private' }
Model >> myDependents [

	^ dependents
]

{ #category : #'dependents-private' }
Model >> myDependents: aCollectionOrNil [

	dependents := aCollectionOrNil
]

{ #category : #updating }
Model >> okToChange [
	"Allows a controller to ask this of any model"
	^ true
]

{ #category : #dependents }
Model >> removeDependent: anObject [
	"Remove the given object as one of the receiver's dependents."

	| newDependents |
	newDependents := self dependents reject: [:each | each == anObject].
	self myDependents: (newDependents isEmpty ifFalse: [ newDependents ]).
	
	^ anObject
]

{ #category : #backstops }
Model >> selectedClass [
	"All owners of TextViews are asked this during a doIt"
	^ nil
]

{ #category : #stepping }
Model >> step [
	"Default for morphic models is no-op"
]

{ #category : #stepping }
Model >> stepAt: millisecondClockValue in: aWindow [

	^ self stepIn: aWindow
]

{ #category : #stepping }
Model >> stepIn: aWindow [

	^ self step
]

{ #category : #stepping }
Model >> stepTime [
	
	^ 1000 "milliseconds -- default backstop for objects serving as models of system windows"
]

{ #category : #stepping }
Model >> stepTimeIn: aSystemWindow [
	
	^ self stepTime 
]

{ #category : #updating }
Model >> update: anAspect [

	^ self
]

{ #category : #updating }
Model >> update: anAspect with: anObject [
	"Receive a change notice from an object of whom the receiver is a 
	dependent. The default behavior is to call update:,
	which by default does nothing; a subclass might want 
	to change itself in some way."

	^ self update: anAspect
]

{ #category : #announcing }
Model >> valueChanged [
	
	"compatibility with the NewValueHolder"
	self announcer announce: (ValueChanged newValue: self)
]

{ #category : #copying }
Model >> veryDeepFixupWith: deepCopier [ 
	"See if the dependents are being copied also.  If so, point at the new copies.  (The dependent has self as its model.)
	Dependents handled in class Object, when the model is not a Model, are fixed up in Object veryDeepCopy."

	| originalDependents refs |
	super veryDeepFixupWith: deepCopier.
	originalDependents := dependents.
	originalDependents ifNil: [
		^self.
		].
	dependents := nil.
	refs := deepCopier references.
	originalDependents
		do: [:originalDependent | | newDependent | 
			newDependent := refs
						at: originalDependent
						ifAbsent: [].
			newDependent
				ifNotNil: [self addDependent: newDependent]]
]

{ #category : #stepping }
Model >> wantsSteps [
	"Overridden by morphic classes whose instances want to be stepped,
	or by model classes who want their morphic views to be stepped."

	^ false
]

{ #category : #stepping }
Model >> wantsStepsIn: aSystemWindow [
	
	^ self wantsSteps
]

{ #category : #announcing }
Model >> whenChangedDo: aBlock [
	"Culled block [ :newValue :oldValue :announcement :announcer | ]"

	| block |
	block := [ :announcement :ann | 
	aBlock
		cull: announcement newValue
		cull: announcement oldValue
		cull: announcement
		cull: ann ].
	self announcer when: ValueChanged do: block
]

{ #category : #announcing }
Model >> whenChangedSend: aSelector to: aReceiver [

	self announcer when: ValueChanged send: aSelector to: aReceiver
]

{ #category : #updating }
Model >> windowIsClosing [
	"This message is used to inform a models that its window is closing. Most models do nothing, but some, such as the Debugger, must do some cleanup. Note that this mechanism must be used with care by models that support multiple views, since one view may be closed while others left open."

]
