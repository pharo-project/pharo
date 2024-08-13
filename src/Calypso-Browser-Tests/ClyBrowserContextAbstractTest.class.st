Class {
	#name : 'ClyBrowserContextAbstractTest',
	#superclass : 'TestCase',
	#instVars : [
		'browser'
	],
	#category : 'Calypso-Browser-Tests',
	#package : 'Calypso-Browser-Tests'
}

{ #category : 'helpers' }
ClyBrowserContextAbstractTest >> allClyContextBut: contexts [

	^ {  ClyFullBrowserPackageContext.
		  ClyFullBrowserClassGroupContext.
		  ClyFullBrowserMethodContext.
		  ClyFullBrowserMethodGroupContext.
		  ClyFullBrowserVariableContext } 
		reject: [ :ctx | contexts includes: ctx ]
]

{ #category : 'helpers' }
ClyBrowserContextAbstractTest >> classContext [

	^ ClyFullBrowserClassContext new
		tool: browser;
		selectedItems: browser classSelection items;
		yourself.
]

{ #category : 'helpers' }
ClyBrowserContextAbstractTest >> classGroupContext [

	^ ClyFullBrowserClassGroupContext new
		tool: browser;
		selectedItems: browser classGroupSelection items;
		yourself.
]

{ #category : 'helpers' }
ClyBrowserContextAbstractTest >> datasourceItemsWith: anObject named: aName [

	^ { ClyDataSourceItem of: nil value: (ClyBrowserItem named: aName with: anObject) }
]

{ #category : 'helpers' }
ClyBrowserContextAbstractTest >> deleteBrowser [

	browser ifNotNil: [ 
		browser window ifNotNil: [ :window | 
			window delete ] ]
]

{ #category : 'helpers' }
ClyBrowserContextAbstractTest >> differentClassGroupContext [

	| items |
	self deleteBrowser.
	browser := ClyFullBrowserMorph open.
	browser selectPackage: (PackageOrganizer default packageNamed: #Kernel) andTag: 'Numbers'.
	items := browser classGroupSelection items.

	^ ClyFullBrowserClassGroupContext new
		  tool: browser;
		  selectedItems: items;
		  yourself
]

{ #category : 'helpers' }
ClyBrowserContextAbstractTest >> differentMethodContext [

	^ self methodContext
		selectedItems: (self datasourceItemsWith: Object>>#= named: #new );
		yourself
]

{ #category : 'helpers' }
ClyBrowserContextAbstractTest >> differentPackageContext [

	^ self packageContext
		selectedItems: (self datasourceItemsWith: Object package named: #Kernel );
		yourself
]

{ #category : 'helpers' }
ClyBrowserContextAbstractTest >> findBrowserTool: aClyToolMorphClass [
	
	^ browser tabManager tools 
		detect: [ :aTool | aTool class = aClyToolMorphClass ]
]

{ #category : 'helpers' }
ClyBrowserContextAbstractTest >> methodContext [

	^ ClyFullBrowserMethodContext new
		selectedItems: browser methodSelection items;
		yourself.
]

{ #category : 'helpers' }
ClyBrowserContextAbstractTest >> methodGroupContext [

	^ ClyFullBrowserMethodGroupContext new
		selectedItems: browser methodGroupSelection items;
		yourself.
]

{ #category : 'helpers' }
ClyBrowserContextAbstractTest >> openBrowserWithPackageAndClassGroupOnlySelected [

	self deleteBrowser.
	browser := ClyFullBrowserMorph open.
	browser selectPackage: (PackageOrganizer default packageNamed: #Kernel) andTag: 'BasicObjects'.
]

{ #category : 'helpers' }
ClyBrowserContextAbstractTest >> openBrowserWithPackageOnlySelected [

	self deleteBrowser.
	browser := ClyFullBrowserMorph open.
	browser selectPackage: self class package.
]

{ #category : 'helpers' }
ClyBrowserContextAbstractTest >> packageContext [

	^ ClyFullBrowserPackageContext new
		tool: browser;
		selectedItems: browser packageSelection items;
		yourself.
]

{ #category : 'running' }
ClyBrowserContextAbstractTest >> setUp [
	super setUp.
	
	browser := ClyFullBrowserMorph 
		openOnClass: self class 
		selector: self class selectors anyOne.
]

{ #category : 'running' }
ClyBrowserContextAbstractTest >> tearDown [ 

	self deleteBrowser.
	super tearDown.
]
