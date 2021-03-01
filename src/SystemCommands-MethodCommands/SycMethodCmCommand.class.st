"
I am a base class for commands (with Commander2) which perform operations with collection of methods.
"
Class {
	#name : #SycMethodCmCommand,
	#superclass : #CmCommand,
	#instVars : [
		'parentGroup'
	],
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #adding }
SycMethodCmCommand class >> addItemsToSubmenu: aCmdMenu forContext: aClyMethodContext [

	| commands |

	aCmdMenu activationStrategyClass = SycMethodMenuActivation ifFalse: [ ^ self ].

	commands := self subclasses
		            collect: [ :e | 
			            e new
				            context: aClyMethodContext;
				            yourself ]
		            thenSelect: [ :e | e isApplicable ].

	commands do: [ :e | aCmdMenu rootGroup addItem: e ]
]

{ #category : #adding }
SycMethodCmCommand class >> addMenuItemsTo: menuMorph onContext: context [ 

	| commands |
	
	commands := SycMethodCmCommand allSubclasses collect: [:aClass | 
		aClass new 
			context: context;
			yourself] thenSelect: [:aCommand | aCommand isApplicable ].

	commands do: [ :aCommand | | anItem |
		anItem := menuMorph add: aCommand name target: aCommand selector: #execute.
		anItem enabled: aCommand isEnabled.
		anItem setBalloonText: aCommand description.
		aCommand icon ifNotNil: [ anItem icon: aCommand icon ].
	].

	^ menuMorph

]

{ #category : #accessing }
SycMethodCmCommand >> icon [

	^ nil
]

{ #category : #testing }
SycMethodCmCommand >> isApplicable [
	
	^ true
]

{ #category : #testing }
SycMethodCmCommand >> isEnabled [

	^ true
]

{ #category : #accessing }
SycMethodCmCommand >> order [
	
	^ 10100
]

{ #category : #accessing }
SycMethodCmCommand >> parentGroup [

	^ parentGroup
]

{ #category : #accessing }
SycMethodCmCommand >> parentGroup: aCmdRootMenuGroup [ 
	
	parentGroup := aCmdRootMenuGroup
]

{ #category : #'context menu' }
SycMethodCmCommand >> registerContextMenuItemsWithBuilder: aBuilder [ 

	(aBuilder item: self name)
		parent: self parentGroup name;
		order: self parentGroup order + (self order / 100);
		help: self description;
		action: [self execute];
		icon: self icon.
]
