"
I represent hierarchy of classes where roots are all common superclasses and children of every class are its subclasses.

In addition I provide default hierarchy for variables and for method visibility.

Variable hierarchy order them by relation between defining classes. I keep default hierarchy in class side variable #hierarchyForVariables.
You can invert it using settings or just by method call: 

	ClySubclassHierarchy invertVariableHierarchy.
	
The method visibility hierarchy is used to represent inherited classes in full browser when you expand first item in third pane. I manage default hierarchy in variable #hierarchyForMethodVisibility.
You can invert it using settings or just by method call: 

	ClySubclassHierarchy invertMethodVisibilityHierarchy
"
Class {
	#name : #ClySubclassHierarchy,
	#superclass : #ClyClassHierarchy,
	#classInstVars : [
		'hierarchyForVariables',
		'hierarchyForMethodVisibility'
	],
	#category : #'Calypso-SystemQueries-Results'
}

{ #category : #'method visibility' }
ClySubclassHierarchy class >> hierarchyForMethodVisibility [
	^hierarchyForMethodVisibility ifNil: [ 
		hierarchyForMethodVisibility := self inverse]
]

{ #category : #'variable hierarchy' }
ClySubclassHierarchy class >> hierarchyForVariables [
	^hierarchyForVariables ifNil: [ 
		hierarchyForVariables := self new]
]

{ #category : #'method visibility' }
ClySubclassHierarchy class >> invertMethodVisibilityHierarchy [
	<script>
	^hierarchyForMethodVisibility := self hierarchyForMethodVisibility inverted
]

{ #category : #'variable hierarchy' }
ClySubclassHierarchy class >> invertVariableHierarchy [
	<script>
	^hierarchyForVariables := self hierarchyForVariables inverted
]

{ #category : #'method visibility' }
ClySubclassHierarchy class >> isDefaultHierarchyForMethodVisibility [
	^self hierarchyForMethodVisibility isInverse not
]

{ #category : #'method visibility' }
ClySubclassHierarchy class >> isDefaultHierarchyForMethodVisibility: aBool [
	self isDefaultHierarchyForMethodVisibility = aBool
		ifFalse: [ self invertMethodVisibilityHierarchy ]
]

{ #category : #'variable hierarchy' }
ClySubclassHierarchy class >> isDefaultHierarchyForVariables [
	^self hierarchyForVariables isInverse not
]

{ #category : #'variable hierarchy' }
ClySubclassHierarchy class >> isDefaultHierarchyForVariables: aBool [
	self isDefaultHierarchyForVariables = aBool 
		ifFalse: [ self invertVariableHierarchy ]
]

{ #category : #'method visibility' }
ClySubclassHierarchy class >> methodVisibilityProvider [
	<classAnnotation>
	^ClyMethodVisibilityProvider new
]

{ #category : #initialization }
ClySubclassHierarchy class >> reset [
	<script>
	hierarchyForMethodVisibility := nil.
	hierarchyForVariables := nil
]

{ #category : #settings }
ClySubclassHierarchy class >> settingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #isDefaultHierarchyForVariables)
		parent: #Calypso;
		label: 'Show variables in standard hierarchical order?';
		default: true;
		description: 'If true, FullBrowser will show variables in the order from superclass to subclass';
		target: self.
	(aBuilder setting: #isDefaultHierarchyForMethodVisibility)
		parent: #Calypso;
		default: false;		
		label: 'Show method visibility classes in standard hierarchical order?';
		description: 'If true, FullBrowser will show "inherited method" expanded classes in the order from superclass to subclass';
		target: self
]

{ #category : #building }
ClySubclassHierarchy >> buildParentMap: aHierarchyMap for: aClass [

	| eachClass superclass |
	eachClass := aClass.
	[eachClass isRootInEnvironment] whileFalse: [ 
		superclass := eachClass superclass.	
		(aHierarchyMap includesClass: superclass) ifTrue: [
			^aHierarchyMap addChild: aClass to: superclass].
		eachClass := superclass]
]
