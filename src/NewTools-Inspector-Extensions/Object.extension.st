Extension { #name : #Object }

{ #category : #'*NewTools-Inspector-Extensions' }
Object >> allInspectorNodes [
	"Answer a list of attributes as nodes"
	
	^ { StInspectorSelfNode hostObject: self }, 
		self inspectorNodes
]

{ #category : #'*NewTools-Inspector-Extensions' }
Object >> defaultObjectInspectorClass [

	^ StObjectInspector
]

{ #category : #'*NewTools-Inspector-Extensions' }
Object >> inspectionPages [
	"This is a utility method that collects all presentations for the current object.
	By default, it simply looks for the #gtInspectorPresentationOrder: pragma.
	The inspector can decice whether or not a presentation should be dispayed.
	Subclasses might want to override it for more special behavior."

	^ (StInspectionCollector on: self) collectPages

]

{ #category : #'*NewTools-Inspector-Extensions' }
Object >> inspectorNodes [
	"Answer a list of attributes as nodes"
	
	^ (StNodeCollector for: self) collectNodes
	  
]

{ #category : #'*NewTools-Inspector-Extensions' }
Object >> metaInspection [
	<inspectorPresentationOrder: 999 title: 'Meta'>

	^ StMetaBrowser on: self
]

{ #category : #'*NewTools-Inspector-Extensions' }
Object >> rawInspection [
	"This is the most basic presentation showing the state of the object"
	<inspectorPresentationOrder: -1 title: 'Raw'>

	^ StRawInspection on: self
]

{ #category : #'*NewTools-Inspector-Extensions' }
Object >> stDisplayString [

	self gtDisplayString linesDo: [ :each | 
		^ each contractTo: 200 ].

	^ ''
]

{ #category : #'*NewTools-Inspector-Extensions' }
Object >> stInspect [
	"Temporary to experiment, should be replaced by inspect later"

	^ StInspector inspect: self
]
