Extension { #name : #Class }

{ #category : #'*GT-InspectorExtensions-Core' }
Class >> gtInspectorAllReferencesIn: composite [
	"This provides a list of all methods provided by the current class"
	<gtInspectorPresentationOrder: 20>
	composite list 
		title: 'All Ref';
		display: [ (SystemNavigation default allReferencesTo: self binding) 
						sorted: [ :x :y | x name < y name ] ];
		format: #name;
		tags: [ :each | { each package name } ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
Class >> gtInspectorAllReferencesOutsideIn: composite [
	"This provides a list of all methods provided by the current class"
	<gtInspectorPresentationOrder: 20>
	composite list 
		title: 'All Ref Outside';
		display: [ ((SystemNavigation default allReferencesTo: self binding) 
							reject: [ :method | method package name == self package name ])
						sorted: [ :x :y | x name < y name ] ];
		format: #name;
		tags: [ :each | { each package name } ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
Class >> gtInspectorCommentIn: composite [
	"This provides an editable comment of the current class"
	<gtInspectorPresentationOrder: 20>
	composite text 
		title: 'Comment';
		display: [ self comment ];
		act: [ :text | 
			self comment: text text stamp: Author changeStamp]
			icon: GLMUIThemeExtraIcons glamorousAccept 
			on: $s
			entitled: 'Accept'
]

{ #category : #'*GT-InspectorExtensions-Core' }
Class >> gtInspectorDefinitionIn: composite [
	<gtInspectorPresentationOrder: 15>
	composite pharoScript 
		title: 'Definition';
		display: [ self definition ] 
]

{ #category : #'*GT-InspectorExtensions-Core' }
Class >> gtInspectorInstanceVariablesIn: composite [
	"This provides a list of all class instance variables and the corresponding values"

	<gtInspectorPresentationOrder: 21>
	composite table
		title: 'InstVars';
		display: [ self class allInstVarNames sorted collect: [ :each | each -> (self instVarNamed: each) ] ];
		column: 'Variable' evaluated: [ :each | each key ];
		column: 'Value' evaluated: [ :each | each value ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
Class >> gtInspectorInstancesOn: composite [
	<gtInspectorPresentationOrder: 40>
	^ composite table
		title: [ 'Instances' translated ];
		display: [ self allInstances ];
		column: 'Hash' evaluated: [ :each | each hash asString ];
		column: 'Object' evaluated: [ :each | each printString ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
Class >> gtInspectorMethodsIn: composite [
	"This provides a list of all methods provided by the current class"

	<gtInspectorPresentationOrder: 20>
	composite list 
		title: 'Methods';
		display: [ (self methods collect: #asRingDefinition) 
						sorted: [ :x :y | x selector < y selector ] ];
		format: #selector;
		tags: [ :each | {each methodClass name} ]
]
