Class {
	#name : #GLMActionsToolbarMorphicTest,
	#superclass : #GLMMorphicTest,
	#category : #'Glamour-Tests-Morphic'
}

{ #category : #tests }
GLMActionsToolbarMorphicTest >> testDynamicActionsAreUpdated [
	<expectedFailure>
	| browser toolbar status count |
	count := 0.
	browser := GLMTabulator new.
	browser addStatusbar.
	browser row: #one.
	browser transmit
		to: #one;
		andShow: [ :composite | 
			composite list
				title: 'List 1';
				dynamicActions: [ :list | 
					| actions |
					status := true.
					actions := OrderedCollection new.
					actions
						add:
							(GLMGenericAction new
								action: [ :presentation :model | browser update ];
								icon: GLMUIThemeExtraIcons glamorousAdd;
								title: count asString;
								yourself).
					actions ] ].
	window := browser openOn: (1 to: 10).
	status := false.
	count := 1.
	browser update.
	self assert: status.	"the dynamic actions are regenerated"
	toolbar := self findLast: ToolDockingBarMorph in: window.
	self assert: toolbar submorphs first model title equals: '1'	"the action has the new model"
]
