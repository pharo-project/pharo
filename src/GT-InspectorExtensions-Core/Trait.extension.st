Extension { #name : #Trait }

{ #category : #'*GT-InspectorExtensions-Core' }
Trait >> gtInspectorAllReferencesIn: composite [
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
Trait >> gtInspectorAllReferencesOutsideIn: composite [
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
Trait >> gtInspectorCommentIn: composite [
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
Trait >> gtInspectorDefinitionIn: composite [
	<gtInspectorPresentationOrder: 15>
	composite pharoScript 
		title: 'Definition';
		display: [ self definition ] 
]

{ #category : #'*GT-InspectorExtensions-Core' }
Trait >> gtInspectorMethodsIn: composite [
	"This provides a list of all methods provided by the current class"
	<gtInspectorPresentationOrder: 20>
	composite list 
		title: 'Methods';
		display: [ (self allMethods collect: #asRing2Definition) 
						sorted: [ :x :y | x selector < y selector ] ];
		format: #selector;
		tags: [ :each | {each methodClass name} ]
]
