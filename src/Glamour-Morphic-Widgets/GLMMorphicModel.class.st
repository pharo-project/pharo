Class {
	#name : #GLMMorphicModel,
	#superclass : #Announcer,
	#traits : 'TViewModel',
	#classTraits : 'TViewModel classTrait',
	#instVars : [
		'glamourPresentation'
	],
	#category : 'Glamour-Morphic-Widgets'
}

{ #category : #callbacks }
GLMMorphicModel >> allKeystrokeActions [
	^ (self glamourPresentation allActions, self glamourPresentation allSelectionActions) select: [ :action | 
		action hasShortcut ]
]

{ #category : #callbacks }
GLMMorphicModel >> allMenuActions [
	^ self glamourPresentation allSelectionActions select: [:action | action hasTitle ]
]

{ #category : #callbacks }
GLMMorphicModel >> executeMenuAction: anAction [
	self announce: (GLMMenuItemSelected action: anAction)
]

{ #category : #accessing }
GLMMorphicModel >> glamourPresentation [
	^ glamourPresentation
]

{ #category : #accessing }
GLMMorphicModel >> glamourPresentation: anObject [
	glamourPresentation := anObject
]

{ #category : #initialization }
GLMMorphicModel >> initialize [
	super initialize.
]

{ #category : #callbacks }
GLMMorphicModel >> keystroke: anEvent from: aMorph [
	| action |

	self deprecated: 'This method is needed only for the text rederer for PluggableTextMorph.'.
	action := self allKeystrokeActions detect: [:a | a shortcut = anEvent keyCharacter] ifNone: [nil].
	action ifNotNil: [ 
		self announce: (GLMKeyStroke action: action).
		^ true ].
	^ false
]

{ #category : #callbacks }
GLMMorphicModel >> menu: aMenuMorph [ 
	| subMenus targetMenuMorph subMenu allActions |
	subMenus := Dictionary new.
	allActions := self allMenuActions.
	allActions do: [ :action |
		targetMenuMorph := action category notNil 
			ifTrue: [ subMenus at: action category ifAbsentPut: [ 
					subMenu :=  MenuMorph new.
					aMenuMorph add: action category subMenu: subMenu.
					subMenu ] ]
			ifFalse: [ aMenuMorph ].
		(action separatorBefore and: [ allActions first ~= action ]) ifTrue: [ targetMenuMorph addLine ].
		(targetMenuMorph 
			add: action title
			target: self
			selector: #executeMenuAction:
			argument: action)
			enabled: (action isEnabledOn: self glamourPresentation);
			icon:  action icon;
			keyText: (action hasShortcut 
				ifTrue: [ action shortcutAsString ]
				ifFalse: [ nil ]).
		(action separatorAfter and: [ allActions last ~= action ]) ifTrue: [ targetMenuMorph addLine ] ].
	^ aMenuMorph
]
