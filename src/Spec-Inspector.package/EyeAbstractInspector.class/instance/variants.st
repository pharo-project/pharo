accessing
variants
	| variants defaultInspectorClass |
	
	variants := OrderedCollection new.
	defaultInspectorClass := self object class inspectorClass.
	
	variants add: defaultInspectorClass. 
	"We only need the basic inspector if we modify something"
	defaultInspectorClass = Object inspectorClass
		ifFalse: [  variants add: EyeBasicInspector ].
	variants add: EyeTreeInspector.
	variants addAll: self object class additionalInspectorClasses. 
	^ variants