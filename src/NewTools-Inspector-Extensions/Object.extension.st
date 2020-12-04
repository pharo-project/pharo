Extension { #name : #Object }

{ #category : #'*NewTools-Inspector-Extensions' }
Object >> allInspectorNodes [
	"Answer a list of attributes as nodes"
	
	^ { StInspectorSelfNode hostObject: self }, 
		self inspectorNodes
]

{ #category : #'*NewTools-Inspector-Extensions' }
Object >> asInspectorModel [

	^ StInspectorModel on: self
]

{ #category : #'*NewTools-Inspector-Extensions' }
Object >> defaultObjectInspectorClass [

	^ StObjectInspectorPresenter
]

{ #category : #'*NewTools-Inspector-Extensions' }
Object >> inspectionContexts [
	"This is a utility method that collects all presentations for the current object.
	By default, it simply looks for the #gtInspectorPresentationOrder: pragma.
	The inspector can decice whether or not a presentation should be dispayed.
	Subclasses might want to override it for more special behavior."

	^ (StInspectionCollector on: self) collectInspectionContexts
]

{ #category : #'*NewTools-Inspector-Extensions' }
Object >> inspectionMeta [
	<inspectorPresentationOrder: 999 title: 'Meta'>

	^ StMetaBrowser on: self
]

{ #category : #'*NewTools-Inspector-Extensions' }
Object >> inspectionMetaContext: aContext [

	aContext withoutEvaluator
]

{ #category : #'*NewTools-Inspector-Extensions' }
Object >> inspectionRaw [
	"This is the most basic presentation showing the state of the object"
	<inspectorPresentationOrder: 900 title: 'Raw'>

	^ StRawInspection on: self
]

{ #category : #'*NewTools-Inspector-Extensions' }
Object >> inspectorNodes [
	"Answer a list of attributes as nodes"
	
	^ (StNodeCollector for: self) collectNodes
]

{ #category : #'*NewTools-Inspector-Extensions' }
Object >> stDisplayString [

	[ self gtDisplayString linesDo: [ :each | ^ each contractTo: 200 ] ]
		on: Error
		do: [ :err | ^ err stDisplayString ].

	^ ''
]

{ #category : #'*NewTools-Inspector-Extensions' }
Object >> stInspect [
	"Temporary to experiment, should be replaced by inspect later"

	^ StInspector inspect: self
]
