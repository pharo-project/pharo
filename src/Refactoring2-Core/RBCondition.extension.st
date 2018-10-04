Extension { #name : #RBCondition }

{ #category : #'*Refactoring2-Core' }
RBCondition class >> accessesClassVariable: variableName in: aClass showIn: aTransformation [

	^ RBCondition withBlock: [ | block |
		block := 	[ :each | 
			(each whichSelectorsReferToClassVariable: variableName)
				ifNotEmpty: [ aClass realClass
					ifNil: [ RBRefactoring new
						refactoringError: ('<1s> is referenced.' expandMacrosWith: variableName) ]
					ifNotNil: [ RBRefactoring new
						refactoringError: ('<1s> is referenced.<n>Browse references?' expandMacrosWith: variableName)
						with: [ aTransformation openBrowserOn: (RBVariableEnvironment 
									referencesToClassVariable: variableName
									in: aClass realClass) ] ] ] ].
			aClass withAllSubclasses do: block.
			aClass theMetaClass withAllSubclasses do: block.
			true ]
]

{ #category : #'*Refactoring2-Core' }
RBCondition class >> accessesInstanceVariable: variableName in: aClass showIn: aTransformation [

	| references |
	references := RBCondition referencesInstanceVariable: variableName in: aClass.
	aClass realClass 
		ifNil: [ references errorMacro: ('<1s> is referenced.' expandMacrosWith: variableName) ]
		ifNotNil: [ references
			errorMacro: ('<1s> is referenced.<n>Browse references?' expandMacrosWith: variableName);
			errorBlock: [ aTransformation openBrowserOn: 
				(RBBrowserEnvironment new instVarRefsTo: variableName in: aClass realClass)]].
	^ references not
]

{ #category : #'*Refactoring2-Core' }
RBCondition class >> doesNotAccessInstanceVariable: variableName inHierarchyOf: aClass showIn: aTransformation [

	| references |
	references := RBCondition hierarchyOf: aClass referencesInstanceVariable: variableName.
	aClass realClass 
		ifNil: [ references errorMacro: ('<1s> is referenced.' expandMacrosWith: variableName) ]
		ifNotNil: [ references
			errorMacro: ('<1s> is referenced.<n>Browse references?' expandMacrosWith: variableName);
			errorBlock: [ aTransformation openBrowserOn: 
				(RBBrowserEnvironment new instVarRefsTo: variableName in: aClass realClass)]].
	^ references not
]
