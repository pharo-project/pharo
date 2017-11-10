Extension { #name : #MetacelloVersion }

{ #category : #'*GT-InspectorExtensions-Core' }
MetacelloVersion >> allProjects [
	| all |
	all := OrderedCollection new.
	self allProjectsDo: [ :each |
		all add: each ].
	^ all
]

{ #category : #'*GT-InspectorExtensions-Core' }
MetacelloVersion >> allProjectsDo: aBlock [
	self allProjectsDo: aBlock withTraversedProjects: OrderedCollection new
]

{ #category : #'*GT-InspectorExtensions-Core' }
MetacelloVersion >> allProjectsDo: aBlock withTraversedProjects: aCollection [
	self projects do: [ :each |
		(each version notNil and: [(aCollection includes: each version) not]) 
		ifTrue: [
			aCollection add: each version.
			aBlock value: each version.
			each version allProjectsDo: aBlock withTraversedProjects: aCollection ] ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
MetacelloVersion >> gtInspectorActionLoad [
	<gtInspectorAction>
	^ GLMGenericAction new
		action: [ self load ];
		iconName: #smallLoadProject;
		title: 'Load'
]

{ #category : #'*GT-InspectorExtensions-Core' }
MetacelloVersion >> gtInspectorPackagesIn: composite [
	<gtInspectorPresentationOrder: 40>
	composite list
		title: 'Packages';
		display: [ (self spec packages packageSpecsInLoadOrderFor: self spec) select: [ :each | 
				each isKindOf: MetacelloPackageSpec ] ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
MetacelloVersion >> gtInspectorProjectsIn: composite [
	<gtInspectorPresentationOrder: 40>
	composite list
		title: 'Projects';
		display: [ self projects ]
]
