"
I am a command to invert hierarhcy in the method visibility class list.
(it is shown when user expands first item in the method groups pane)
"
Class {
	#name : 'ClyInvertMethodVisibilityHierarchyCommand',
	#superclass : 'SycMethodGroupExtraCmCommand',
	#category : 'Calypso-SystemTools-FullBrowser-Commands-MethodGroups',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Commands-MethodGroups'
}

{ #category : 'execution' }
ClyInvertMethodVisibilityHierarchyCommand >> executeRefactoring [

	ClySubclassHierarchy invertMethodVisibilityHierarchy.
	self context browser switchToMethodGroups
]

{ #category : 'testing' }
ClyInvertMethodVisibilityHierarchyCommand >> isApplicable [

	^ context isAboutBrowserItemType: ClyInheritedMethodGroup
]

{ #category : 'accessing' }
ClyInvertMethodVisibilityHierarchyCommand >> name [

	^ 'Invert hierarchy'
]

{ #category : 'accessing' }
ClyInvertMethodVisibilityHierarchyCommand >> order [

	^ 1000
]
