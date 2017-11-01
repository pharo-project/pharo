"
I'm an example of how metalinks can be used to show the coverage of an execution.

self new openWithSpec
"
Class {
	#name : #CoverageDemo,
	#superclass : #ComposablePresenter,
	#instVars : [
		'installMetalinkButton',
		'runCodeButton',
		'codePanelMorph',
		'metalinkBlock',
		'compiledMethods',
		'coverageCode'
	],
	#category : 'Reflectivity-Examples'
}

{ #category : #specs }
CoverageDemo class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed
		newColumn: [:col|
			col add: #codePanelMorph.
			col newRow: [ :row|
				row add: #installMetalinkButton.
				row add: #runCodeButton.	
			] height:  (self toolbarHeight)
		]
]

{ #category : #specs }
CoverageDemo class >> title [
	^'Coverage Demo'
]

{ #category : #adding }
CoverageDemo >> addCards [

	compiledMethods do:[:method|
		codePanelMorph widget addMorph: 
			(ReflectivityCodeMorph compiledMethod: method)
	]
	
]

{ #category : #accessing }
CoverageDemo >> codePanelMorph [
	^ codePanelMorph
]

{ #category : #accessing }
CoverageDemo >> codePanelMorph: anObject [
	codePanelMorph := anObject
]

{ #category : #accessing }
CoverageDemo >> compiledMethods [
	^ compiledMethods
]

{ #category : #accessing }
CoverageDemo >> compiledMethods: anObject [
	compiledMethods := anObject
]

{ #category : #accessing }
CoverageDemo >> coverageCode [
	^ coverageCode
]

{ #category : #accessing }
CoverageDemo >> coverageCode: anObject [
	coverageCode := anObject
]

{ #category : #example }
CoverageDemo >> exampleMethod [

	self openWithSpec.
]

{ #category : #example }
CoverageDemo >> exampleNotCompleteCoverage [

	self coverageCode: [ ReflectivityExamples new exampleIfTrueIfFalse. ].
	self compiledMethods add: ReflectivityExamples>>#exampleIfTrueIfFalse.
	
	self openWithSpec.
]

{ #category : #api }
CoverageDemo >> initialExtent [
	^ 700@600
]

{ #category : #initialization }
CoverageDemo >> initialize [

	coverageCode := [ReflectivityExamples new exampleMethod].
	compiledMethods := OrderedCollection new.
	compiledMethods add: ReflectivityExamples>>#exampleMethod.
	
	super initialize.
]

{ #category : #initialization }
CoverageDemo >> initializePresenter [

	installMetalinkButton action: [self installMetalink].
	runCodeButton action: [ self runCoverageCode ].
	
	"on close"
	self whenWindowChanged: [  
		self window whenClosedDo: [ self removeMetalink ]
	].
]

{ #category : #initialization }
CoverageDemo >> initializeWidgets [

	installMetalinkButton := self newButton.
	runCodeButton := self newButton.
	codePanelMorph := ReflectivitySourceCodeAgroupation new asSpecAdapter.
	
	installMetalinkButton label: 'Install Metalink'.
	runCodeButton label: 'Run Example Code'.
]

{ #category : #action }
CoverageDemo >> installMetalink [
	|link|
	
	link := MetaLink new
		metaObject: #node;
		selector: #tagExecuted.

	compiledMethods do:[:method| method ast nodesDo: [:node | node link: link]].
]

{ #category : #accessing }
CoverageDemo >> installMetalinkButton [
	^ installMetalinkButton
]

{ #category : #accessing }
CoverageDemo >> installMetalinkButton: anObject [
	installMetalinkButton := anObject
]

{ #category : #accessing }
CoverageDemo >> metalinkBlock [
	^ metalinkBlock
]

{ #category : #accessing }
CoverageDemo >> metalinkBlock: anObject [
	metalinkBlock := anObject
]

{ #category : #api }
CoverageDemo >> openWithSpec [
	self addCards.
	^ super openWithSpec
]

{ #category : #action }
CoverageDemo >> removeMetalink [
	
	compiledMethods do:[:method| method methodClass recompile].
	
]

{ #category : #accessing }
CoverageDemo >> runCodeButton [
	^ runCodeButton
]

{ #category : #accessing }
CoverageDemo >> runCodeButton: anObject [
	runCodeButton := anObject
]

{ #category : #action }
CoverageDemo >> runCoverageCode [

	coverageCode value.
	self updateRubricStyle.
]

{ #category : #styling }
CoverageDemo >> updateRubricStyle [

	codePanelMorph widget submorphs do:[:card| |conf ast|
			
		"delete previous styling"
		conf := RubConfigurationChange new.
		conf configurationBlock: 
			[:text | text segments copy do:#delete].
		card textModel announce: conf.
			
		"add new styling"
		ast := card compiledMethod ast.
		(ast children anySatisfy: [ :node| node hasBeenExecuted])
			ifTrue:[
				ast acceptVisitor: (CoverageRubricStyler new textModel: card textModel)
			]
	]
]
