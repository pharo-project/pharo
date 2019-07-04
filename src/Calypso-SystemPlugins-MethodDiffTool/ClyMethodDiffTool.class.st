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
	#name : #ClyMethodDiffTool,
	#superclass : #ClyBrowserToolMorph,
	#instVars : [
		'diffMorph',
		'leftMethod',
		'rightMethod'
	],
	#category : #'Calypso-SystemPlugins-MethodDiffTool'
}

{ #category : #activation }
ClyMethodDiffTool class >> browserTabActivation [
	"This declaration specifies that in any browser when methods are selected, a diff tool will be available in a tab."

	<classAnnotation>
	^ClyTabActivationStrategy for: ClyMethod asCalypsoItemContext
]

{ #category : #accessing }
ClyMethodDiffTool class >> shouldBeActivatedInContext: aBrowserContext [
	^aBrowserContext selectedMethods size > 1
]

{ #category : #accessing }
ClyMethodDiffTool class >> tabOrder [
	^10000
]

{ #category : #accessing }
ClyMethodDiffTool >> activationPriority [
	^0
]

{ #category : #controlling }
ClyMethodDiffTool >> attachToSystem [

	browser system 
		when: (ClyMethodChange ofAll: {self leftMethod. self rightMethod}) 
		send: #triggerUpdate to: self
]

{ #category : #testing }
ClyMethodDiffTool >> belongsToCurrentBrowserContext [
	^(browser isMethodSelected: leftMethod) 
		and: [ browser isMethodSelected: rightMethod ]
]

{ #category : #testing }
ClyMethodDiffTool >> belongsToRemovedBrowserContext [
	^self leftMethod isInstalled not or: [ self rightMethod isInstalled not ]
]

{ #category : #building }
ClyMethodDiffTool >> build [
	diffMorph := DiffMorph from: self leftMethod sourceCode to: self rightMethod sourceCode.
	diffMorph	
		contextClass: self rightMethod sourceCode;
		hResizing:	#spaceFill;
		vResizing: #spaceFill;
		showOptions: false.
	self addMorph: diffMorph fullFrame: LayoutFrame identity
]

{ #category : #initialization }
ClyMethodDiffTool >> defaultIconName [
	^#diff
]

{ #category : #initialization }
ClyMethodDiffTool >> defaultTitle [
	^'Diff'
]

{ #category : #controlling }
ClyMethodDiffTool >> detachFromSystem [

	browser system unsubscribe: self
]

{ #category : #testing }
ClyMethodDiffTool >> isSimilarTo: anotherBrowserTool [
	(super isSimilarTo: anotherBrowserTool) ifFalse: [ ^false ].
	
	^{leftMethod. rightMethod} asIdentitySet = {anotherBrowserTool leftMethod. anotherBrowserTool rightMethod  } asIdentitySet
]

{ #category : #accessing }
ClyMethodDiffTool >> leftMethod [
	^leftMethod
]

{ #category : #printing }
ClyMethodDiffTool >> printContext [
	^'left / ', leftMethod printSystemPath , ' vs right / ', rightMethod printSystemPath
]

{ #category : #initialization }
ClyMethodDiffTool >> resetStateForSnapshot [
	super resetStateForSnapshot.
	
	diffMorph := nil
]

{ #category : #accessing }
ClyMethodDiffTool >> rightMethod [
	^rightMethod
]

{ #category : #initialization }
ClyMethodDiffTool >> setUpModelFromContext [ 

	leftMethod := context firstSelectedMethod.
	rightMethod := context lastSelectedMethod
]

{ #category : #updating }
ClyMethodDiffTool >> update [
	leftMethod := leftMethod origin 
		localMethodNamed: leftMethod selector ifAbsent: [^self].
	rightMethod := rightMethod origin 
		localMethodNamed: rightMethod selector ifAbsent: [^self].
	
	super update.
	
	diffMorph from: leftMethod sourceCode to: rightMethod sourceCode
]
