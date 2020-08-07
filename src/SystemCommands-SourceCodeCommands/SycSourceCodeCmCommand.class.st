"
This class is an extension so we can mix command of Commander2 with Commander.
"
Class {
	#name : #SycSourceCodeCmCommand,
	#superclass : #CmCommand,
	#instVars : [
		'parentGroup'
	],
	#category : #'SystemCommands-SourceCodeCommands'
}

{ #category : #'as yet unclassified' }
SycSourceCodeCmCommand class >> addItemsToSubmenu: aCmdMenu forContext: aClyMethodSourceCodeContext [

	| commands |

	aCmdMenu activationStrategyClass = SycSourceCodeMenuActivation ifFalse: [ ^ self ].

	commands := self subclasses
		            collect: [ :e | 
			            e new
				            context: aClyMethodSourceCodeContext;
				            yourself ]
		            thenSelect: [ :e | e isApplicable ].

	commands do: [ :e | aCmdMenu rootGroup addItem: e ]
]

{ #category : #'menu creation' }
SycSourceCodeCmCommand class >> addMenuItemsTo: menuMorph onContext: context [ 

	| commands |
	
	commands := SycSourceCodeCmCommand allSubclasses collect: [:aClass | 
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
SycSourceCodeCmCommand >> icon [

	^ nil
]

{ #category : #testing }
SycSourceCodeCmCommand >> isApplicable [
	
	^ true
]

{ #category : #accessing }
SycSourceCodeCmCommand >> isEnabled [

	^ true
]

{ #category : #'as yet unclassified' }
SycSourceCodeCmCommand >> order [
	
	^ 100
]

{ #category : #'as yet unclassified' }
SycSourceCodeCmCommand >> parentGroup [

	^ parentGroup
]

{ #category : #'as yet unclassified' }
SycSourceCodeCmCommand >> parentGroup: aCmdRootMenuGroup [ 
	
	parentGroup := aCmdRootMenuGroup
]

{ #category : #'as yet unclassified' }
SycSourceCodeCmCommand >> registerContextMenuItemsWithBuilder: aBuilder [ 

	(aBuilder item: self name)
		parent: self parentGroup name;
		order: self parentGroup order + (self order / 100);
		help: self description;
		action: [self execute];
		icon: self icon.
]
