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
Object >> changed [
	"Receiver changed in a general way; inform all the dependents by
	sending each dependent an update: message."

	self changed: self
]

{ #category : '*System-Model' }
Object >> changed: aParameter [
	"Receiver changed. The change is denoted by the argument aParameter.
	Usually the argument is a Symbol that is part of the dependent's change
	protocol. Inform all of the dependents."

	self dependents do: [:aDependent | aDependent update: aParameter]
]

{ #category : '*System-Model' }
Object >> changed: anAspect with: anObject [
	"Receiver changed. The change is denoted by the argument anAspect.
	Usually the argument is a Symbol that is part of the dependent's change
	protocol. Inform all of the dependents. Also pass anObject for additional information."

	self dependents do: [:aDependent | aDependent update: anAspect with: anObject]
]

{ #category : '*System-Model' }
Object >> dependents [
	"Answer a collection of objects that are 'dependent' on the receiver;
	 that is, all objects that should be notified if the receiver changes."

	^ self myDependents
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

{ #category : '*System-Model' }
Object >> update: aParameter [
	"Receive a change notice from an object of whom the receiver is a
	dependent. The default behavior is to do nothing; a subclass might want
	to change itself in some way."

	^ self
]

{ #category : '*System-Model' }
Object >> update: anAspect with: anObject [
	"Receive a change notice from an object of whom the receiver is a
	dependent. The default behavior is to call update:,
	which by default does nothing; a subclass might want
	to change itself in some way."

	^ self update: anAspect
]

{ #category : '*System-Model' }
Object >> veryDeepCopy [
	"Do a complete tree copy using a dictionary.  An object in the tree twice is only copied once.  All references to the object in the copy of the tree will point to the new copy."

	| copier new |
	copier := DeepCopier new.
	new := self veryDeepCopyWith: copier.
	copier references associationsDo: [:assoc |
		assoc value veryDeepFixupWith: copier].
	copier fixDependents.
	^ new
]

{ #category : '*System-Model' }
Object >> veryDeepCopyWith: deepCopier [
	"Copy me and the entire tree of objects I point to.  An object in the tree twice is copied once, and both references point to him.  deepCopier holds a dictionary of objects we have seen.  Some classes refuse to be copied.  Some classes are picky about which fields get deep copied."

	| class selfNumberOfInstanceVariables fieldOfSelf copyOfSelf currentClass hasVeryDeepInnerMethod currentNumberOfInstanceVariables |

	deepCopier references at: self ifPresent: [ :newer | 	"already copied" ^ newer].
	class := self class.
	class isMeta ifTrue: [ "a metaclass" ^ self ].
	copyOfSelf := self shallowCopy.
	deepCopier references at: self put: copyOfSelf.	"remember"

	(class isVariable and: [class isPointers]) ifTrue: [
		self basicSize to: 1 by: -1 do: [ :i |
			fieldOfSelf := self basicAt: i.
			copyOfSelf
				basicAt: i
				put: (deepCopier references
					at: fieldOfSelf
					ifAbsent: [ fieldOfSelf veryDeepCopyWith: deepCopier ])]].

	"Ask each superclass if it wants to share (weak copy) any inst vars"
	copyOfSelf veryDeepInner: deepCopier.

	"other superclasses want all instance variables deep copied"
	currentClass := class.
	selfNumberOfInstanceVariables := class instSize.

	[ selfNumberOfInstanceVariables == 0 ] whileFalse: [
		hasVeryDeepInnerMethod := currentClass includesSelector: #veryDeepInner:.
		currentNumberOfInstanceVariables := currentClass instSize - currentClass superclass instSize.
		hasVeryDeepInnerMethod
			ifTrue: ["skip inst vars"
				selfNumberOfInstanceVariables := selfNumberOfInstanceVariables - currentNumberOfInstanceVariables]
			ifFalse: [
				currentNumberOfInstanceVariables timesRepeat: [
					fieldOfSelf := self instVarAt: selfNumberOfInstanceVariables.
					copyOfSelf
						instVarAt: selfNumberOfInstanceVariables
						put: (deepCopier references
							at: fieldOfSelf
							ifAbsent: [ fieldOfSelf veryDeepCopyWith: deepCopier ]).
					selfNumberOfInstanceVariables := selfNumberOfInstanceVariables - 1 ]].
		currentClass := currentClass superclass ].

	^ copyOfSelf
]

{ #category : '*System-Model' }
Object >> veryDeepFixupWith: deepCopier [
	"I have no fields and no superclass.  Catch the super call."

	"avoid to use me we will deprecate it in the future"
]

{ #category : '*System-Model' }
Object >> veryDeepInner: deepCopier [
	"No special treatment for inst vars of my superclasses.  Override when some need to be weakly copied.  Object>>veryDeepCopyWith: will veryDeepCopy any inst var whose class does not actually define veryDeepInner:"

	"avoid to use me we will deprecate it in the future"
]
