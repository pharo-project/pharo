Class {
	#name : 'ClyBrowserToolValidityTest',
	#superclass : 'ClyBrowserContextAbstractTest',
	#instVars : [
		'tool',
		'context'
	],
	#category : 'Calypso-Browser-Tests',
	#package : 'Calypso-Browser-Tests'
}

{ #category : 'testing' }
ClyBrowserToolValidityTest >> isValidInContextOtherThan: someContexts [

	^ (self allClyContextBut: someContexts) 
		anySatisfy: [ :ctx | tool isValidInContext: ctx ]
]

{ #category : 'accessing' }
ClyBrowserToolValidityTest >> mockPackage [

	^ self packageOrganizer ensurePackage: self mockPackageName
]

{ #category : 'accessing' }
ClyBrowserToolValidityTest >> mockPackageName [

	^ 'MockPackageForTestingCalypso'.
]

{ #category : 'running' }
ClyBrowserToolValidityTest >> tearDown [

	self packageOrganizer removePackage: self mockPackageName.

	super tearDown
]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testClassCommentToolIsNotValidWhenNotAClassContext [

	tool := self findBrowserTool: ClyRichTextClassCommentEditorToolMorph.
	
	self deny: (self isValidInContextOtherThan: { ClyFullBrowserClassContext }).
]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testClassCommentToolIsNotValidWhenReferencingDifferentClass [

	tool := self findBrowserTool: ClyRichTextClassCommentEditorToolMorph.
	context := self classContext
		selectedItems: (self datasourceItemsWith: Object named: #Object );
		yourself.
	
	self deny: (tool isValidInContext: context).
]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testClassCommentToolIsValidWhenReferencingSameClass [

	tool := self findBrowserTool: ClyRichTextClassCommentEditorToolMorph.
	
	self assert: (tool isValidInContext: self classContext).
]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testClassCreationToolIsNotValidWhenNotAPackageOrClassGroupContext [
	
	self openBrowserWithPackageOnlySelected.
	tool := self findBrowserTool: ClyClassCreationToolMorph.
	
	self deny: (self isValidInContextOtherThan: { ClyFullBrowserPackageContext . ClyFullBrowserClassGroupContext }).
]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testClassCreationToolIsNotValidWhenReferencingDifferentClassGroup [

	self openBrowserWithPackageAndClassGroupOnlySelected.
	tool := self findBrowserTool: ClyClassCreationToolMorph.
	
	self deny: (tool isValidInContext: self differentClassGroupContext).
]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testClassCreationToolIsNotValidWhenReferencingDifferentPackage [

	self openBrowserWithPackageOnlySelected.
	tool := self findBrowserTool: ClyClassCreationToolMorph.
	
	self deny: (tool isValidInContext: self differentPackageContext).
]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testClassCreationToolIsValidWhenClassGroupContext [

	self openBrowserWithPackageAndClassGroupOnlySelected.
	tool := self findBrowserTool: ClyClassCreationToolMorph.
	
	self assert: (tool isValidInContext: self classGroupContext).
]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testClassCreationToolIsValidWhenPackageContext [

	self openBrowserWithPackageOnlySelected.
	tool := self findBrowserTool: ClyClassCreationToolMorph.
	
	self assert: (tool isValidInContext: self packageContext).
]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testClassDefinitionToolIsNotValidWhenNotAClassContext [

	tool := self findBrowserTool: ClyClassDefinitionEditorToolMorph.
	
	self deny: (self isValidInContextOtherThan: { ClyFullBrowserClassContext }).
]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testClassDefinitionToolIsNotValidWhenReferencingDifferentClass [

	tool := self findBrowserTool: ClyClassDefinitionEditorToolMorph.
	context := self classContext
		selectedItems: (self datasourceItemsWith: Object named: #Object );
		yourself.
	
	self deny: (tool isValidInContext: context).
]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testClassDefinitionToolIsValidWhenClassContext [

	tool := self findBrowserTool: ClyClassDefinitionEditorToolMorph.
	
	self assert: (tool isValidInContext: self classContext).
]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testMethodEditorToolIsNotValidWhenNotAMethodContext [

	tool := self findBrowserTool: ClyMethodCodeEditorToolMorph.
	
	self deny: (self isValidInContextOtherThan: { ClyFullBrowserMethodContext }).
]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testMethodEditorToolIsNotValidWhenReferencingDifferentMethod [

	tool := self findBrowserTool: ClyMethodCodeEditorToolMorph.
	
	self deny: (tool isValidInContext: self differentMethodContext).
]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testMethodEditorToolIsValidWhenReferencingSameMethod [

	tool := self findBrowserTool: ClyMethodCodeEditorToolMorph.
	
	self assert: (tool isValidInContext: self methodContext).
]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testPackageCommentToolIsNotValidWhenNotAPackageOrClassGroupContext [

	self openBrowserWithPackageOnlySelected.
	tool := self findBrowserTool: ClyPackageRichTextCommentEditorToolMorph.
	
	self deny: (self isValidInContextOtherThan: { ClyFullBrowserPackageContext . ClyFullBrowserClassGroupContext }).
]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testPackageCommentToolIsNotValidWhenReferencingDifferentPackage [

	self openBrowserWithPackageOnlySelected.
	tool := self findBrowserTool: ClyPackageRichTextCommentEditorToolMorph.
	
	self deny: (tool isValidInContext: self differentPackageContext).
]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testPackageCommentToolIsValidWhenClassGroupContext [

	self openBrowserWithPackageAndClassGroupOnlySelected.
	tool := self findBrowserTool: ClyPackageRichTextCommentEditorToolMorph.
	
	self assert: (tool isValidInContext: self classGroupContext).
]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testPackageCommentToolIsValidWhenReferencingDifferentClassGroup [

	self openBrowserWithPackageAndClassGroupOnlySelected.
	tool := self findBrowserTool: ClyPackageRichTextCommentEditorToolMorph.
	
	self assert: (tool isValidInContext: self differentClassGroupContext).
]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testPackageCommmentToolIsValidWhenPackageContext [

	self openBrowserWithPackageOnlySelected.
	tool := self findBrowserTool: ClyPackageRichTextCommentEditorToolMorph.
		
	self assert: (tool isValidInContext: self packageContext).
]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testPackageSelectionAfterPackageCreation [

	self deleteBrowser.
	browser := ClyFullBrowserMorph openOnPackage: self mockPackage.
	tool := self findBrowserTool: ClyClassCreationToolMorph.
	
	self 
		assertCollection: (self packageContext selectedPackageItems collect: #name)
		equals: (Array with: self mockPackageName).

]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testSetUpToolIsNotValidWhenNotAClassContext [

	tool := self findBrowserTool: ClyTestSetUpEditorToolMorph.
	
	self deny: (self isValidInContextOtherThan: { ClyFullBrowserClassContext }).
]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testSetUpToolIsNotValidWhenReferencingDifferentClass [

	tool := self findBrowserTool: ClyTestSetUpEditorToolMorph.
	context := self classContext
		selectedItems: (self datasourceItemsWith: Object named: #Object );
		yourself.
	
	self deny: (tool isValidInContext: context).
]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testSetUpToolIsValidWhenReferencingSameClass [

	tool := self findBrowserTool: ClyTestSetUpEditorToolMorph.
	
	self assert: (tool isValidInContext: self classContext).
]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testWindowLabelIsUpdatedAfterPackageRemoval [

	| realPackage command activator cmdContext |
	
	self deleteBrowser.
	browser := ClyFullBrowserMorph openOnPackage: self mockPackage.
	tool := self findBrowserTool: ClyClassCreationToolMorph.
	
	realPackage := self packageContext selectedPackageItems anyOne actualObject.
	
	command := SycRemovePackageCommand new
		packages: { realPackage };
		yourself.
	cmdContext := browser createCommandContext. 

	activator := CmdCommandActivator new 
		command: command;
		context: cmdContext;
		yourself.

	cmdContext executeCommand: command by: activator.
	activator applyCommandResult.
	self 
		assert: browser window label 
		equals: 'Current image'
]

{ #category : 'tests' }
ClyBrowserToolValidityTest >> testWindowLabelSetAfterPackageCreation [

	self deleteBrowser.
	browser := ClyFullBrowserMorph openOnPackage: self mockPackage.
	tool := self findBrowserTool: ClyClassCreationToolMorph.

	self assert: browser window label equals: 'Package: ' , self mockPackageName
]
