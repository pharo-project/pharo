Extension { #name : #Object }

{ #category : #'*System-Object Events' }
Object >> actionForEvent: anEventSelector [
    "Answer the action to be evaluated when <anEventSelector> has been triggered."

	| actions |
	actions := self actionMap
		at: anEventSelector asSymbol
		ifAbsent: [^nil].
 
	^ actions asMinimalRepresentation
]

{ #category : #'*System-Object Events' }
Object >> actionForEvent: anEventSelector
ifAbsent: anExceptionBlock [
    "Answer the action to be evaluated when <anEventSelector> has been triggered."

	| actions |
	actions := self actionMap
		at: anEventSelector asSymbol
		ifAbsent: [nil].
	actions ifNil: [^anExceptionBlock value].
	^ actions asMinimalRepresentation
]

{ #category : #'*System-Object Events' }
Object >> actionMap [

	^EventManager actionMapFor: self
]

{ #category : #'*System-Object Events' }
Object >> actionSequenceForEvent: anEventSelector [

    ^(self actionMap
        at: anEventSelector asSymbol
        ifAbsent: [^WeakActionSequence new])
            asActionSequence
]

{ #category : #'*System-Object Events' }
Object >> actionsDo: aBlock [

	self actionMap do: aBlock
]

{ #category : #'*System-Object Events' }
Object >> asActionSequence [

	^WeakActionSequence with: self
]

{ #category : #'*System-Object Events' }
Object >> createActionMap [

	^IdentityDictionary new
]

{ #category : #'*System-Object Events' }
Object >> hasActionForEvent: anEventSelector [
    "Answer true if there is an action associated with anEventSelector"

    ^(self actionForEvent: anEventSelector) notNil
]

{ #category : #'*System-Object Events' }
Object >> hasActionsWithReceiver: anObject [

	^self actionMap keys anySatisfy:
		[:eachEventSelector |
			(self actionSequenceForEvent: eachEventSelector)
				anySatisfy: [:anAction | anAction receiver == anObject]]
]

{ #category : #'*System-Object Events' }
Object >> releaseActionMap [

	EventManager releaseActionMapFor: self
]

{ #category : #'*System-Object Events' }
Object >> removeAction: anAction
forEvent: anEventSelector [

    self
        removeActionsSatisfying: [:action | action = anAction]
        forEvent: anEventSelector
]

{ #category : #'*System-Object Events' }
Object >> removeActionsForEvent: anEventSelector [

    | map |
    map := self actionMap.
    map removeKey: anEventSelector asSymbol ifAbsent: [].
    map isEmpty
        ifTrue: [self releaseActionMap]
]

{ #category : #'*System-Object Events' }
Object >> removeActionsSatisfying: aBlock [

	self actionMap keysDo:
		[:eachEventSelector |
			self
   				removeActionsSatisfying: aBlock
				forEvent: eachEventSelector
		]
]

{ #category : #'*System-Object Events' }
Object >> removeActionsSatisfying: aOneArgBlock 
forEvent: anEventSelector [

    self
        setActionSequence:
            ((self actionSequenceForEvent: anEventSelector)
                reject: [:anAction | aOneArgBlock value: anAction])
        forEvent: anEventSelector
]

{ #category : #'*System-Object Events' }
Object >> removeActionsWithReceiver: anObject [

	self actionMap copy keysDo:
		[:eachEventSelector |
			self
   				removeActionsSatisfying: [:anAction | anAction receiver == anObject]
				forEvent: eachEventSelector
		]
]

{ #category : #'*System-Object Events' }
Object >> removeActionsWithReceiver: anObject
forEvent: anEventSelector [

    self
        removeActionsSatisfying:
            [:anAction |
            anAction receiver == anObject]
        forEvent: anEventSelector
]

{ #category : #'*System-Object Events' }
Object >> setActionSequence: actionSequence
forEvent: anEventSelector [

    | action |
    action := actionSequence asMinimalRepresentation.
    action 
	  ifNil:
            [self removeActionsForEvent: anEventSelector]
        ifNotNil:
            [self updateableActionMap
                at: anEventSelector asSymbol
                put: action]
]

{ #category : #'*System-Object Events' }
Object >> triggerEvent: anEventSelector [
	"Evaluate all actions registered for <anEventSelector>. Return the value of the last registered action."

    ^(self actionForEvent: anEventSelector) value
]

{ #category : #'*System-Object Events' }
Object >> triggerEvent: anEventSelector
with: anObject [

    ^self 
		triggerEvent: anEventSelector
		withArguments: { anObject }
]

{ #category : #'*System-Object Events' }
Object >> triggerEvent: anEventSelector
withArguments: anArgumentList [

    ^(self actionForEvent: anEventSelector)
        valueWithArguments: anArgumentList
]

{ #category : #'*System-Object Events' }
Object >> updateableActionMap [

	^EventManager updateableActionMapFor: self
]

{ #category : #'*System-Object Events' }
Object >> when: anEventSelector evaluate: anAction [ 

	| actions |
	actions := self actionSequenceForEvent: anEventSelector.
	(actions includes: anAction)
		ifTrue: [^ self].
	self 
		setActionSequence: (actions copyWith: anAction)
		forEvent: anEventSelector
]

{ #category : #'*System-Object Events' }
Object >> when: anEventSelector
send: aMessageSelector
to: anObject [
 
    self
        when: anEventSelector
        evaluate: (WeakMessageSend
            receiver: anObject
            selector: aMessageSelector)
]

{ #category : #'*System-Object Events' }
Object >> when: anEventSelector
send: aMessageSelector
to: anObject
with: anArg [
 
    self
        when: anEventSelector
        evaluate: (WeakMessageSend
            receiver: anObject
            selector: aMessageSelector
		arguments: {anArg})
]

{ #category : #'*System-Object Events' }
Object >> when: anEventSelector
send: aMessageSelector
to: anObject
withArguments: anArgArray [
 
    self
        when: anEventSelector
        evaluate: (WeakMessageSend
            receiver: anObject
            selector: aMessageSelector
		arguments: anArgArray)
]
