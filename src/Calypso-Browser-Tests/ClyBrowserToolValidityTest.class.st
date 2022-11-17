Class {
	#name : #ClyBrowserToolValidityTest,
	#superclass : #TestCase,
	#instVars : [
		'browser',
		'tool',
		'context'
	],
	#category : #'Calypso-Browser-Tests'
}

{ #category : #helpers }
ClyBrowserToolValidityTest >> allClyContextBut: contexts [

	^ {  ClyFullBrowserPackageContext.
		  ClyFullBrowserClassGroupContext.
		  ClyFullBrowserMethodContext.
		  ClyFullBrowserMethodGroupContext.
		  ClyFullBrowserVariableContext } 
		reject: [ :ctx | contexts includes: ctx ]
]

{ #category : #helpers }
ClyBrowserToolValidityTest >> classContext [

	^ ClyFullBrowserClassContext new
		tool: browser;
		selectedItems: browser classSelection items;
		yourself.
]

{ #category : #helpers }
ClyBrowserToolValidityTest >> classGroupContext [

	^ ClyFullBrowserClassGroupContext new
		tool: browser;
		selectedItems: browser classGroupSelection items;
		yourself.
]

{ #category : #helpers }
ClyBrowserToolValidityTest >> datasourceItemsWith: anObject named: aName [

	^ { ClyDataSourceItem of: nil value: (ClyBrowserItem named: aName with: anObject) }
]

{ #category : #helpers }
ClyBrowserToolValidityTest >> deleteBrowser [

	browser ifNotNil: [ 
		browser window ifNotNil: [ :window | 
			window delete ] ]
]

{ #category : #helpers }
ClyBrowserToolValidityTest >> differentClassGroupContext [
	| items |
	
	self deleteBrowser.
	browser := ClyFullBrowserMorph open.
	browser selectPackage: (RPackageOrganizer default packageNamed: #Kernel) atClassTag: 'Chronology'.
	items := browser classGroupSelection items.

	^ ClyFullBrowserClassGroupContext new
		tool: browser;
		selectedItems: items;
		yourself.
]

{ #category : #helpers }
ClyBrowserToolValidityTest >> differentMethodContext [

	^ self methodContext
		selectedItems: (self datasourceItemsWith: Object>>#= named: #new );
		yourself
]

{ #category : #helpers }
ClyBrowserToolValidityTest >> differentPackageContext [

	^ self packageContext
		selectedItems: (self datasourceItemsWith: Object package named: #Kernel );
		yourself
]

{ #category : #helpers }
ClyBrowserToolValidityTest >> findBrowserTool: aClyToolMorphClass [
	
	^ browser tabManager tools 
		detect: [ :aTool | aTool class = aClyToolMorphClass ]
]

{ #category : #testing }
ClyBrowserToolValidityTest >> isValidInContextOtherThan: someContexts [

	^ (self allClyContextBut: someContexts) 
		anySatisfy: [ :ctx | tool isValidInContext: ctx ]
]

{ #category : #helpers }
ClyBrowserToolValidityTest >> methodContext [

	^ ClyFullBrowserMethodContext new
		selectedItems: browser methodSelection items;
		yourself.
]

{ #category : #helpers }
ClyBrowserToolValidityTest >> methodGroupContext [

	^ ClyFullBrowserMethodGroupContext new
		selectedItems: browser methodGroupSelection items;
		yourself.
]

{ #category : #helpers }
ClyBrowserToolValidityTest >> openBrowserWithPackageAndClassGroupOnlySelected [

	self deleteBrowser.
	browser := ClyFullBrowserMorph open.
	browser selectPackage: (RPackageOrganizer default packageNamed: #Kernel) atClassTag: 'BasicObjects'.
]

{ #category : #helpers }
ClyBrowserToolValidityTest >> openBrowserWithPackageOnlySelected [

	self deleteBrowser.
	browser := ClyFullBrowserMorph open.
	browser selectPackage: self class package.
]

{ #category : #helpers }
ClyBrowserToolValidityTest >> packageContext [

	^ ClyFullBrowserPackageContext new
		tool: browser;
		selectedItems: browser packageSelection items;
		yourself.
]

{ #category : #running }
ClyBrowserToolValidityTest >> setUp [
	super setUp.
	
	browser := ClyFullBrowserMorph 
		openOnClass: self class 
		selector: self class selectors anyOne.
]

{ #category : #running }
ClyBrowserToolValidityTest >> tearDown [ 

	self deleteBrowser
]

{ #category : #tests }
ClyBrowserToolValidityTest >> testClassCommentToolIsNotValidWhenNotAClassContext [

	tool := self findBrowserTool: ClyRichTextClassCommentEditorToolMorph.
	
	self deny: (self isValidInContextOtherThan: { ClyFullBrowserClassContext }).
]

{ #category : #tests }
ClyBrowserToolValidityTest >> testClassCommentToolIsNotValidWhenReferencingDifferentClass [

	tool := self findBrowserTool: ClyRichTextClassCommentEditorToolMorph.
	context := self classContext
		selectedItems: (self datasourceItemsWith: Object named: #Object );
		yourself.
	
	self deny: (tool isValidInContext: context).
]

{ #category : #tests }
ClyBrowserToolValidityTest >> testClassCommentToolIsValidWhenReferencingSameClass [

	tool := self findBrowserTool: ClyRichTextClassCommentEditorToolMorph.
	
	self assert: (tool isValidInContext: self classContext).
]

{ #category : #tests }
ClyBrowserToolValidityTest >> testClassCreationToolIsNotValidWhenNotAPackageOrClassGroupContext [
	
	self openBrowserWithPackageOnlySelected.
	tool := self findBrowserTool: ClyClassCreationToolMorph.
	
	self deny: (self isValidInContextOtherThan: { ClyFullBrowserPackageContext . ClyFullBrowserClassGroupContext }).
]

{ #category : #tests }
ClyBrowserToolValidityTest >> testClassCreationToolIsNotValidWhenReferencingDifferentClassGroup [

	self openBrowserWithPackageAndClassGroupOnlySelected.
	tool := self findBrowserTool: ClyClassCreationToolMorph.
	
	self deny: (tool isValidInContext: self differentClassGroupContext).
]

{ #category : #tests }
ClyBrowserToolValidityTest >> testClassCreationToolIsNotValidWhenReferencingDifferentPackage [

	self openBrowserWithPackageOnlySelected.
	tool := self findBrowserTool: ClyClassCreationToolMorph.
	
	self deny: (tool isValidInContext: self differentPackageContext).
]

{ #category : #tests }
ClyBrowserToolValidityTest >> testClassCreationToolIsValidWhenClassGroupContext [

	self openBrowserWithPackageAndClassGroupOnlySelected.
	tool := self findBrowserTool: ClyClassCreationToolMorph.
	
	self assert: (tool isValidInContext: self classGroupContext).
]

{ #category : #tests }
ClyBrowserToolValidityTest >> testClassCreationToolIsValidWhenPackageContext [

	self openBrowserWithPackageOnlySelected.
	tool := self findBrowserTool: ClyClassCreationToolMorph.
	
	self assert: (tool isValidInContext: self packageContext).
]

{ #category : #tests }
ClyBrowserToolValidityTest >> testClassDefinitionToolIsNotValidWhenNotAClassContext [

	tool := self findBrowserTool: ClyClassDefinitionEditorToolMorph.
	
	self deny: (self isValidInContextOtherThan: { ClyFullBrowserClassContext }).
]

{ #category : #tests }
ClyBrowserToolValidityTest >> testClassDefinitionToolIsNotValidWhenReferencingDifferentClass [

	tool := self findBrowserTool: ClyClassDefinitionEditorToolMorph.
	context := self classContext
		selectedItems: (self datasourceItemsWith: Object named: #Object );
		yourself.
	
	self deny: (tool isValidInContext: context).
]

{ #category : #tests }
ClyBrowserToolValidityTest >> testClassDefinitionToolIsValidWhenClassContext [

	tool := self findBrowserTool: ClyClassDefinitionEditorToolMorph.
	
	self assert: (tool isValidInContext: self classContext).
]

{ #category : #tests }
ClyBrowserToolValidityTest >> testMethodEditorToolIsNotValidWhenNotAMethodContext [

	tool := self findBrowserTool: ClyMethodCodeEditorToolMorph.
	
	self deny: (self isValidInContextOtherThan: { ClyFullBrowserMethodContext }).
]

{ #category : #tests }
ClyBrowserToolValidityTest >> testMethodEditorToolIsNotValidWhenReferencingDifferentMethod [

	tool := self findBrowserTool: ClyMethodCodeEditorToolMorph.
	
	self deny: (tool isValidInContext: self differentMethodContext).
]

{ #category : #tests }
ClyBrowserToolValidityTest >> testMethodEditorToolIsValidWhenReferencingSameMethod [

	tool := self findBrowserTool: ClyMethodCodeEditorToolMorph.
	
	self assert: (tool isValidInContext: self methodContext).
]

{ #category : #tests }
ClyBrowserToolValidityTest >> testPackageCommentToolIsNotValidWhenNotAPackageOrClassGroupContext [

	self openBrowserWithPackageOnlySelected.
	tool := self findBrowserTool: ClyPackageRichTextCommentEditorToolMorph.
	
	self deny: (self isValidInContextOtherThan: { ClyFullBrowserPackageContext . ClyFullBrowserClassGroupContext }).
]

{ #category : #tests }
ClyBrowserToolValidityTest >> testPackageCommentToolIsNotValidWhenReferencingDifferentPackage [

	self openBrowserWithPackageOnlySelected.
	tool := self findBrowserTool: ClyPackageRichTextCommentEditorToolMorph.
	
	self deny: (tool isValidInContext: self differentPackageContext).
]

{ #category : #tests }
ClyBrowserToolValidityTest >> testPackageCommentToolIsValidWhenClassGroupContext [

	self openBrowserWithPackageAndClassGroupOnlySelected.
	tool := self findBrowserTool: ClyPackageRichTextCommentEditorToolMorph.
	
	self assert: (tool isValidInContext: self classGroupContext).
]

{ #category : #tests }
ClyBrowserToolValidityTest >> testPackageCommentToolIsValidWhenReferencingDifferentClassGroup [

	self openBrowserWithPackageAndClassGroupOnlySelected.
	tool := self findBrowserTool: ClyPackageRichTextCommentEditorToolMorph.
	
	self assert: (tool isValidInContext: self differentClassGroupContext).
]

{ #category : #tests }
ClyBrowserToolValidityTest >> testPackageCommmentToolIsValidWhenPackageContext [

	self openBrowserWithPackageOnlySelected.
	tool := self findBrowserTool: ClyPackageRichTextCommentEditorToolMorph.
		
	self assert: (tool isValidInContext: self packageContext).
]

{ #category : #tests }
ClyBrowserToolValidityTest >> testSetUpToolIsNotValidWhenNotAClassContext [

	tool := self findBrowserTool: ClyTestSetUpEditorToolMorph.
	
	self deny: (self isValidInContextOtherThan: { ClyFullBrowserClassContext }).
]

{ #category : #tests }
ClyBrowserToolValidityTest >> testSetUpToolIsNotValidWhenReferencingDifferentClass [

	tool := self findBrowserTool: ClyTestSetUpEditorToolMorph.
	context := self classContext
		selectedItems: (self datasourceItemsWith: Object named: #Object );
		yourself.
	
	self deny: (tool isValidInContext: context).
]

{ #category : #tests }
ClyBrowserToolValidityTest >> testSetUpToolIsValidWhenReferencingSameClass [

	tool := self findBrowserTool: ClyTestSetUpEditorToolMorph.
	
	self assert: (tool isValidInContext: self classContext).
]
