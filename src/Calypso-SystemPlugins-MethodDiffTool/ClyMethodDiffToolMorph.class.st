"
I a browser tool to compare two methods.
I am automatically activated when two methods are selected.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	diffMorph:		<DiffMorph>
	leftMethod:		<CompiledMethod>
	rightMethod:		<CompiledMethod>
"
Class {
	#name : #ClyMethodDiffToolMorph,
	#superclass : #ClyBrowserToolMorph,
	#instVars : [
		'diffMorph',
		'leftMethod',
		'rightMethod'
	],
	#category : #'Calypso-SystemPlugins-MethodDiffTool'
}

{ #category : #activation }
ClyMethodDiffToolMorph class >> browserTabActivation [
	"This declaration specifies that in any browser when methods are selected, a diff tool will be available in a tab."

	<classAnnotation>
	^ClyTabActivationStrategyAnnotation for: ClyMethod asCalypsoItemContext
]

{ #category : #accessing }
ClyMethodDiffToolMorph class >> shouldBeActivatedInContext: aBrowserContext [
	^aBrowserContext selectedMethods size > 1
]

{ #category : #accessing }
ClyMethodDiffToolMorph class >> tabOrder [
	^10000
]

{ #category : #accessing }
ClyMethodDiffToolMorph >> activationPriority [
	^0
]

{ #category : #controlling }
ClyMethodDiffToolMorph >> attachToSystem [

	browser system 
		when: (ClyMethodChange ofAll: {self leftMethod. self rightMethod}) 
		send: #triggerUpdate to: self
]

{ #category : #testing }
ClyMethodDiffToolMorph >> belongsToCurrentBrowserContext [
	^(browser isMethodSelected: leftMethod) 
		and: [ browser isMethodSelected: rightMethod ]
]

{ #category : #testing }
ClyMethodDiffToolMorph >> belongsToRemovedBrowserContext [
	^self leftMethod isInstalled not or: [ self rightMethod isInstalled not ]
]

{ #category : #building }
ClyMethodDiffToolMorph >> build [
	diffMorph := DiffMorph from: self leftMethod sourceCode to: self rightMethod sourceCode.
	diffMorph	
		contextClass: self rightMethod sourceCode;
		hResizing:	#spaceFill;
		vResizing: #spaceFill;
		showOptions: false.
	self addMorph: diffMorph fullFrame: LayoutFrame identity
]

{ #category : #initialization }
ClyMethodDiffToolMorph >> defaultIconName [
	^#diff
]

{ #category : #initialization }
ClyMethodDiffToolMorph >> defaultTitle [
	^'Diff'
]

{ #category : #controlling }
ClyMethodDiffToolMorph >> detachFromSystem [

	browser system unsubscribe: self
]

{ #category : #testing }
ClyMethodDiffToolMorph >> isSimilarTo: anotherBrowserTool [
	(super isSimilarTo: anotherBrowserTool) ifFalse: [ ^false ].
	
	^{leftMethod. rightMethod} asIdentitySet = {anotherBrowserTool leftMethod. anotherBrowserTool rightMethod  } asIdentitySet
]

{ #category : #accessing }
ClyMethodDiffToolMorph >> leftMethod [
	^leftMethod
]

{ #category : #printing }
ClyMethodDiffToolMorph >> printContext [
	^'left / ', leftMethod printSystemPath , ' vs right / ', rightMethod printSystemPath
]

{ #category : #initialization }
ClyMethodDiffToolMorph >> resetStateForSnapshot [
	super resetStateForSnapshot.
	
	diffMorph := nil
]

{ #category : #accessing }
ClyMethodDiffToolMorph >> rightMethod [
	^rightMethod
]

{ #category : #initialization }
ClyMethodDiffToolMorph >> setUpModelFromContext [ 

	leftMethod := context firstSelectedMethod.
	rightMethod := context lastSelectedMethod
]

{ #category : #updating }
ClyMethodDiffToolMorph >> update [
	leftMethod := leftMethod origin 
		localMethodNamed: leftMethod selector ifAbsent: [^self].
	rightMethod := rightMethod origin 
		localMethodNamed: rightMethod selector ifAbsent: [^self].
	
	super update.
	
	diffMorph from: leftMethod sourceCode to: rightMethod sourceCode
]
