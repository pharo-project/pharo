"
I'm an example of how metalinks can be used to show the coverage of an execution.

CoverageDemo open
"
Class {
	#name : 'CoverageDemoPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'installMetalinkButton',
		'runCodeButton',
		'codePanelMorph',
		'metalinkBlock',
		'compiledMethods',
		'coverageCode'
	],
	#category : 'Reflectivity-Examples',
	#package : 'Reflectivity-Examples'
}

{ #category : 'layout' }
CoverageDemoPresenter class >> defaultLayout [
	^ SpBoxLayout newTopToBottom
		add: #codePanelMorph;
		add:
			(SpBoxLayout newLeftToRight
				add: #installMetalinkButton;
				add: #runCodeButton;
				yourself)
			withConstraints: [ :constraints | constraints height: self toolbarHeight ];
		yourself
]

{ #category : 'accessing' }
CoverageDemoPresenter class >> defaultPreferredExtent [

	^ 700 @ 600
]

{ #category : 'examples' }
CoverageDemoPresenter class >> exampleIfTrueIfFalse [
	| t |
	t := false.
	t ifTrue: [ self bar ] ifFalse: [ 'hello' ].
	^ 5
]

{ #category : 'examples' }
CoverageDemoPresenter class >> exampleMethod [
	^ 2 + 3
]

{ #category : 'open' }
CoverageDemoPresenter class >> open [
	<script>
	(self newApplication: StPharoApplication current) open
]

{ #category : 'adding' }
CoverageDemoPresenter >> addCards [
	compiledMethods do: [ :method | codePanelMorph morph addMorph: (ReflectivityCodeMorph compiledMethod: method) ]
]

{ #category : 'accessing' }
CoverageDemoPresenter >> compiledMethods [
	^ compiledMethods
]

{ #category : 'accessing' }
CoverageDemoPresenter >> compiledMethods: anObject [
	compiledMethods := anObject
]

{ #category : 'accessing' }
CoverageDemoPresenter >> coverageCode [
	^ coverageCode
]

{ #category : 'accessing' }
CoverageDemoPresenter >> coverageCode: anObject [
	coverageCode := anObject
]

{ #category : 'examples' }
CoverageDemoPresenter >> exampleMethod [

	self open
]

{ #category : 'examples' }
CoverageDemoPresenter >> exampleNotCompleteCoverage [

	self coverageCode: [ CoverageDemoPresenter exampleIfTrueIfFalse. ].
	self compiledMethods add: CoverageDemoPresenter class >>#exampleIfTrueIfFalse.

	self open
]

{ #category : 'initialization' }
CoverageDemoPresenter >> initialize [
	coverageCode := [ CoverageDemoPresenter exampleMethod ].
	compiledMethods := OrderedCollection new.
	compiledMethods add: CoverageDemoPresenter class >> #exampleMethod.

	super initialize
]

{ #category : 'initialization' }
CoverageDemoPresenter >> initializePresenter [
	installMetalinkButton action: [ self installMetalink ].
	runCodeButton action: [ self runCoverageCode ]
]

{ #category : 'initialization' }
CoverageDemoPresenter >> initializeWidgets [
	installMetalinkButton := self newButton.
	runCodeButton := self newButton.
	codePanelMorph := self newMorph.

	codePanelMorph morph: ReflectivitySourceCodeAgroupation new.

	installMetalinkButton label: 'Install Metalink'.
	runCodeButton label: 'Run Example Code'
]

{ #category : 'initialization' }
CoverageDemoPresenter >> initializeWindow: aWindowPresenter [

	super initializeWindow: aWindowPresenter.
	aWindowPresenter
		whenClosedDo: [ self removeMetalink ]
]

{ #category : 'action' }
CoverageDemoPresenter >> installMetalink [
	| link |
	link := MetaLink new
		metaObject: #node;
		selector: #tagExecuted.

	compiledMethods do: [ :method | method ast nodesDo: [ :node | node link: link ] ]
]

{ #category : 'accessing' }
CoverageDemoPresenter >> metalinkBlock [
	^ metalinkBlock
]

{ #category : 'accessing' }
CoverageDemoPresenter >> metalinkBlock: anObject [
	metalinkBlock := anObject
]

{ #category : 'api' }
CoverageDemoPresenter >> open [
	self addCards.
	^ super open
]

{ #category : 'action' }
CoverageDemoPresenter >> removeMetalink [

	compiledMethods do:[:method| method methodClass recompile]
]

{ #category : 'action' }
CoverageDemoPresenter >> runCoverageCode [

	coverageCode value.
	self updateRubricStyle
]

{ #category : 'styling' }
CoverageDemoPresenter >> updateRubricStyle [
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

{ #category : 'initialization' }
CoverageDemoPresenter >> windowTitle [
	^'Coverage Demo'
]
