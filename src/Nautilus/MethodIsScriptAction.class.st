"
Action when the method is an executable script tagged with  the following pragma

someMethod
  <script>
  ...
"
Class {
	#name : #MethodIsScriptAction,
	#superclass : #AbstractMethodIconAction,
	#category : 'Nautilus-Widgets'
}

{ #category : #order }
MethodIsScriptAction >> actionOrder [
	"Return the priority of this action"
	
	^0
]

{ #category : #order }
MethodIsScriptAction >> actionStateToCache [
    "Return the state of the icon for caching purpose"
   ^ icon
]

{ #category : #testing }
MethodIsScriptAction >> isActionHandled [

	^ method methodClass isMeta
		and: [ method numArgs isZero and: [ method pragmas anySatisfy: [ :each | each keyword = #script ] ] ]
]

{ #category : #'private - accessing' }
MethodIsScriptAction >> pragmaName [
	"The name of the pragma we handle"
	
	 ^#script
]

{ #category : #'private - accessing' }
MethodIsScriptAction >> privateActionIcon [

	 ^IconicButton new 
			target: self;
			actionSelector: #runScript;
			labelGraphic: (self iconNamed: #scriptManagerIcon) ;
			color: Color transparent;
			extent: self iconExtent;
			helpText: 'Run the script(s)';
			borderWidth: 0 
]

{ #category : #'private - actions' }
MethodIsScriptAction >> runScript [
	method pragmas
		select: [ :pragma | pragma keyword = self pragmaName ]
		thenDo: [ :pragma | 
			self runScriptForPragma: pragma.
			UIManager default inform: 'Script executed: ' , method name asString ]
		
	 
]

{ #category : #'private - actions' }
MethodIsScriptAction >> runScriptForPragma: pragma [

	method methodClass theNonMetaClass perform: method selector.
	 
]
