"
A MorphicWindowManagerTest is a test class for testing the behavior of MorphicWindowManager
"
Class {
	#name : 'MorphicWindowManagerTest',
	#superclass : 'AbstractWindowManagerTest',
	#category : 'Morphic-Tests',
	#package : 'Morphic-Tests'
}

{ #category : 'tests' }
MorphicWindowManagerTest >> testDeleteANonOpenWindowFails [

	| newWindow |
	newWindow := manager newWindowWithRoot: Morph new.

	[ 
	newWindow delete.
	self fail ]
		on: Error
		do: [ :err | 
			self
				assert: err messageText
				equals: 'Cannot delete non open window' ]
]

{ #category : 'tests' }
MorphicWindowManagerTest >> testDeleteAWindow [

	| newWindow |
	
	newWindow := manager newWindowWithRoot: Morph new.

	newWindow ensureDelete .

	self assert: manager numberOfOpenWindows equals: 0
]

{ #category : 'tests' }
MorphicWindowManagerTest >> testDeleteAWindowAndTaskBarActualized [

	| newWindow |
	"This test do not pass on CI because headless image has display size = 0@0 and then 
	 morphic behaves weird. PropagateNaturalHeight/Width is weak on Morphic because of 
	 this (if not, see the algoritm I was forced to do), but for now passes (just, not on CI :)"
	Smalltalk isInteractiveGraphic ifFalse: [ ^ self skip ].
	
	manager := MorphicWindowManager open.

	newWindow := manager openNewWindowWithRoot: Morph new.

	self assert: manager taskBar submorphs size equals: 1.


	newWindow delete.

	self assert: manager taskBar submorphs size equals: 0
]

{ #category : 'tests' }
MorphicWindowManagerTest >> testDeleteAllWindow [

	manager newWindowWithRoot: Morph new.
	manager newWindowWithRoot: Morph new.
	manager newWindowWithRoot: Morph new.
	manager newWindowWithRoot: Morph new.

	manager ensureCloseAllWindows.

	self assert: manager numberOfOpenWindows equals: 0
]

{ #category : 'tests' }
MorphicWindowManagerTest >> testDeleteAllWindowToLeftOfAWindow [

	| newWindow |
	"This test do not pass on CI because headless image has display size = 0@0 and then 
	 morphic behaves weird. PropagateNaturalHeight/Width is weak on Morphic because of 
	 this (if not, see the algoritm I was forced to do), but for now passes (just, not on CI :)"
	Smalltalk isInteractiveGraphic ifFalse: [ ^ self skip ].

	manager openNewWindowWithRoot: Morph new.
	manager openNewWindowWithRoot: Morph new.
	manager openNewWindowWithRoot: Morph new.
	manager openNewWindowWithRoot: Morph new.
	newWindow := manager openNewWindowWithRoot: Morph new.

	newWindow closeAllWindowToLeft.

	self assert: manager numberOfOpenWindows equals: 1
]

{ #category : 'tests' }
MorphicWindowManagerTest >> testDeleteAllWindowToLeftOfAWindowWhenDontHaveWindowOnLeft [

	| newWindow |
	newWindow := manager newWindowWithRoot: Morph new.

	newWindow closeAllWindowToLeft.

	self assert: manager numberOfOpenWindows equals: 1
]

{ #category : 'tests' }
MorphicWindowManagerTest >> testDeleteAllWindowToRightOfAWindow [

	| newWindow |
	"This test do not pass on CI because headless image has display size = 0@0 and then 
	 morphic behaves weird. PropagateNaturalHeight/Width is weak on Morphic because of 
	 this (if not, see the algoritm I was forced to do), but for now passes (just, not on CI :)"
	Smalltalk isInteractiveGraphic ifFalse: [ ^ self skip ].

	newWindow := manager openNewWindowWithRoot: Morph new.
	manager openNewWindowWithRoot: Morph new.
	manager openNewWindowWithRoot: Morph new.
	manager openNewWindowWithRoot: Morph new.
	manager openNewWindowWithRoot: Morph new.

	newWindow closeAllWindowToRight.

	self assert: manager numberOfOpenWindows equals: 1
]

{ #category : 'tests' }
MorphicWindowManagerTest >> testDeleteAllWindowToRightOfAWindowWhenDontHaveWindowOnRight [

	| newWindow |
	newWindow := manager newWindowWithRoot: Morph new.

	newWindow closeAllWindowToRight.

	self assert: manager numberOfOpenWindows equals: 1
]

{ #category : 'tests' }
MorphicWindowManagerTest >> testNewWindowShouldBeSave [

	| newWindow |
	newWindow := manager newWindowWithRoot: Morph new.

	self assert: manager numberOfWindows equals: 1
]

{ #category : 'tests' }
MorphicWindowManagerTest >> testWindowShouldBeOpenAfterStartupWithGoodMorphWhenNoMorphIsSave [

	manager saveAllRoot.

	manager deleteAll.

	manager startupRoot.

	self assert: manager numberOfWindows equals: 0
]

{ #category : 'tests' }
MorphicWindowManagerTest >> testWindowShouldBeSaveBeforeShutDownSaveAttributesButNoRoot [

	manager saveAllAttributes.

	self assert: manager numberOfAttributesSave equals: 0
]

{ #category : 'tests' }
MorphicWindowManagerTest >> testWindowShouldBeSaveBeforeShutDownSaveRoot [

	manager newWindowWithRoot: Morph new.
	manager newWindowWithRoot: Morph new.

	manager saveAllRoot.

	self assert: manager numberOfRootSave equals: 2
]

{ #category : 'tests' }
MorphicWindowManagerTest >> testWindowShouldBeSaveBeforeShutDownSaveRootButHaveNotRoot [

	manager saveAllRoot.

	self assert: manager numberOfRootSave equals: 0
]
