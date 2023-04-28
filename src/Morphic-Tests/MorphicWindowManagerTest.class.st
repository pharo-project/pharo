"
A MorphicWindowManagerTest is a test class for testing the behavior of MorphicWindowManager
"
Class {
	#name : #MorphicWindowManagerTest,
	#superclass : #AbstractWindowManagerTest,
	#category : #'Morphic-Tests'
}

{ #category : #tests }
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

{ #category : #tests }
MorphicWindowManagerTest >> testDeleteAWindow [

	| newWindow |
	newWindow := manager openNewWindowWithRoot: Morph new.

	newWindow delete.

	self assert: manager numberOfOpenWindows equals: 0
]

{ #category : #tests }
MorphicWindowManagerTest >> testDeleteAWindowAndTaskBarActualized [

	| newWindow |
	newWindow := manager openNewWindowWithRoot: Morph new.

	newWindow delete.

	self assert: manager taskBar submorphs size equals: 0
]

{ #category : #tests }
MorphicWindowManagerTest >> testDeleteAll [

	| newWindow |

	newWindow := manager openNewWindowWithRoot: Morph new.

	manager deleteAll.

	self assert: manager numberOfOpenWindows equals: 0.

	self deny: manager isTaskBarOpen
]

{ #category : #tests }
MorphicWindowManagerTest >> testDeleteAllWindow [

	| newWindow |
	newWindow := manager openNewWindowWithRoot: Morph new.
	manager openNewWindowWithRoot: Morph new.
	manager openNewWindowWithRoot: Morph new.
	manager openNewWindowWithRoot: Morph new.
	manager openNewWindowWithRoot: Morph new.

	manager closeAllWindows.

	self assert: manager numberOfOpenWindows equals: 0
]

{ #category : #tests }
MorphicWindowManagerTest >> testDeleteAllWindowToLeftOfAWindow [

	| newWindow |
	manager openNewWindowWithRoot: Morph new.
	manager openNewWindowWithRoot: Morph new.
	manager openNewWindowWithRoot: Morph new.
	manager openNewWindowWithRoot: Morph new.
	newWindow := manager openNewWindowWithRoot: Morph new.

	newWindow closeAllWindowToLeft.

	self assert: manager numberOfOpenWindows equals: 1
]

{ #category : #tests }
MorphicWindowManagerTest >> testDeleteAllWindowToLeftOfAWindowWhenDontHaveWindowOnLeft [

	| newWindow |
	newWindow := manager openNewWindowWithRoot: Morph new.

	newWindow closeAllWindowToLeft.

	self assert: manager numberOfOpenWindows equals: 1
]

{ #category : #tests }
MorphicWindowManagerTest >> testDeleteAllWindowToRightOfAWindow [

	| newWindow |
	newWindow := manager openNewWindowWithRoot: Morph new.
	manager openNewWindowWithRoot: Morph new.
	manager openNewWindowWithRoot: Morph new.
	manager openNewWindowWithRoot: Morph new.
	manager openNewWindowWithRoot: Morph new.

	newWindow closeAllWindowToRight.

	self assert: manager numberOfOpenWindows equals: 1
]

{ #category : #tests }
MorphicWindowManagerTest >> testDeleteAllWindowToRightOfAWindowWhenDontHaveWindowOnRight [

	| newWindow |
	newWindow := manager openNewWindowWithRoot: Morph new.

	newWindow closeAllWindowToRight.

	self assert: manager numberOfOpenWindows equals: 1
]

{ #category : #tests }
MorphicWindowManagerTest >> testNewWindowShouldBeSave [

	| newWindow |
	newWindow := manager newWindowWithRoot: Morph new.

	self assert: manager numberOfWindows equals: 1
]

{ #category : #tests }
MorphicWindowManagerTest >> testOpenNewWindowShouldBeSave [

	| newWindow |
	newWindow := manager openNewWindowWithRoot:
		             ClyFullBrowserMorph onDefaultEnvironment.
	self assert: manager numberOfOpenWindows equals: 1
]

{ #category : #tests }
MorphicWindowManagerTest >> testWindowShouldBeOpenAfterStartupWithGoodMorphWhenNoMorphIsSave [

	manager saveAllRoot.

	manager deleteAll.

	manager startupRoot.

	self assert: manager numberOfWindows equals: 0
]

{ #category : #tests }
MorphicWindowManagerTest >> testWindowShouldBeOpenAfterStartupWithtaskBar [

	manager deleteAll.

	manager startupTaskbar.

	self assert: manager isTaskBarOpen
]

{ #category : #tests }
MorphicWindowManagerTest >> testWindowShouldBeSaveBeforeShutDownSaveAttributes [

	manager openNewWindowWithRoot: Morph new.
	manager saveAllAttributes.
	
	self assert: manager numberOfAttributesSave equals: 1.
	

	

]

{ #category : #tests }
MorphicWindowManagerTest >> testWindowShouldBeSaveBeforeShutDownSaveAttributesButNoRoot [

	manager saveAllAttributes.

	self assert: manager numberOfAttributesSave equals: 0
]

{ #category : #tests }
MorphicWindowManagerTest >> testWindowShouldBeSaveBeforeShutDownSaveRoot [

	manager openNewWindowWithRoot: Morph new.
	manager openNewWindowWithRoot: Morph new.

	manager saveAllRoot.

	self assert: manager numberOfRootSave equals: 2
]

{ #category : #tests }
MorphicWindowManagerTest >> testWindowShouldBeSaveBeforeShutDownSaveRootButHaveNotRoot [

	manager saveAllRoot.

	self assert: manager numberOfRootSave equals: 0
]
