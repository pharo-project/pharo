"
I'm an example of how metalinks can be used to show the coverage of an execution.

CoverageDemo open
"
Class {
	#name : #CoverageDemo,
	#superclass : #SpPresenter,
	#instVars : [
		'installMetalinkButton',
		'runCodeButton',
		'codePanelMorph',
		'metalinkBlock',
		'compiledMethods',
		'coverageCode'
	],
	#category : #'Reflectivity-Examples'
}

{ #category : #specs }
CoverageDemo class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #codePanelMorph;
		add:
			(SpBoxLayout newHorizontal
				add: #installMetalinkButton;
				add: #runCodeButton;
				yourself)
			withConstraints: [ :constraints | constraints height: self toolbarHeight ];
		yourself
]

{ #category : #example }
CoverageDemo class >> exampleIfTrueIfFalse [
	| t |
	t := false.
	t ifTrue: [ self bar ] ifFalse: [ 'hello' ].
	^ 5 
]

{ #category : #example }
CoverageDemo class >> exampleMethod [
	^ 2 + 3
]

{ #category : #open }
CoverageDemo class >> open [
	<script>
	self new openWithSpec
]

{ #category : #specs }
CoverageDemo class >> title [
	^'Coverage Demo'
]

{ #category : #adding }
CoverageDemo >> addCards [
	compiledMethods do: [ :method | codePanelMorph morph addMorph: (ReflectivityCodeMorph compiledMethod: method) ]
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

	self coverageCode: [ CoverageDemo exampleIfTrueIfFalse. ].
	self compiledMethods add: CoverageDemo class >>#exampleIfTrueIfFalse.
	
	self openWithSpec.
]

{ #category : #initialization }
CoverageDemo >> initialize [
	coverageCode := [ CoverageDemo exampleMethod ].
	compiledMethods := OrderedCollection new.
	compiledMethods add: CoverageDemo class >> #exampleMethod.

	super initialize
]

{ #category : #initialization }
CoverageDemo >> initializePresenter [
	installMetalinkButton action: [ self installMetalink ].
	runCodeButton action: [ self runCoverageCode ]
]

{ #category : #initialization }
CoverageDemo >> initializeWidgets [
	installMetalinkButton := self newButton.
	runCodeButton := self newButton.
	codePanelMorph := self newMorph.

	codePanelMorph morph: ReflectivitySourceCodeAgroupation new.

	installMetalinkButton label: 'Install Metalink'.
	runCodeButton label: 'Run Example Code'
]

{ #category : #initialization }
CoverageDemo >> initializeWindow: aWindowPresenter [
	aWindowPresenter
		initialExtent: 700 @ 600;
		whenClosedDo: [ self removeMetalink ]
]

{ #category : #action }
CoverageDemo >> installMetalink [
	| link |
	link := MetaLink new
		metaObject: #node;
		selector: #tagExecuted.

	compiledMethods do: [ :method | method ast nodesDo: [ :node | node link: link ] ]
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

{ #category : #action }
CoverageDemo >> runCoverageCode [

	coverageCode value.
	self updateRubricStyle.
]

{ #category : #styling }
CoverageDemo >> updateRubricStyle [
	codePanelMorph morph submorphs
		do: [ :card | 
			| conf ast |
			"delete previous styling"
			conf := RubConfigurationChange new.
			conf configurationBlock: [ :text | text segments copy do: #delete ].
			card textModel announce: conf.

			"add new styling"
			ast := card compiledMethod ast.
			(ast children anySatisfy: [ :node | node hasBeenExecuted ]) ifTrue: [ ast acceptVisitor: (CoverageRubricStyler new textModel: card textModel) ] ]
]
